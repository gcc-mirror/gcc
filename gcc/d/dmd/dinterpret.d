/**
 * The entry point for CTFE.
 *
 * Specification: ($LINK2 https://dlang.org/spec/function.html#interpretation, Compile Time Function Execution (CTFE))
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dinterpret.d, _dinterpret.d)
 * Documentation:  https://dlang.org/phobos/dmd_dinterpret.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dinterpret.d
 */

module dmd.dinterpret;

import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.builtin;
import dmd.constfold;
import dmd.ctfeexpr;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.initsem;
import dmd.location;
import dmd.mtype;
import dmd.root.rmem;
import dmd.root.array;
import dmd.root.ctfloat;
import dmd.root.region;
import dmd.rootobject;
import dmd.root.utf;
import dmd.statement;
import dmd.tokens;
import dmd.typesem : mutableOf, equivalent, pointerTo, sarrayOf, arrayOf, size;
import dmd.utils : arrayCastBigEndian;
import dmd.visitor;

/*************************************
 * Entry point for CTFE.
 * A compile-time result is required. Give an error if not possible.
 *
 * `e` must be semantically valid expression. In other words, it should not
 * contain any `ErrorExp`s in it. But, CTFE interpretation will cross over
 * functions and may invoke a function that contains `ErrorStatement` in its body.
 * If that, the "CTFE failed because of previous errors" error is raised.
 */
public Expression ctfeInterpret(Expression e)
{
    switch (e.op)
    {
        case EXP.int64:
        case EXP.float64:
        case EXP.complex80:
        case EXP.null_:
        case EXP.void_:
        case EXP.string_:
        case EXP.this_:
        case EXP.super_:
        case EXP.type:
        case EXP.typeid_:
        case EXP.template_:              // non-eponymous template/instance
        case EXP.scope_:                 // ditto
        case EXP.dotTemplateDeclaration: // ditto, e.e1 doesn't matter here
        case EXP.dotTemplateInstance:    // ditto
        case EXP.dot:                    // ditto
             if (e.type.ty == Terror)
                return ErrorExp.get();
            goto case EXP.error;

        case EXP.error:
            return e;

        default:
            break;
    }

    assert(e.type); // https://issues.dlang.org/show_bug.cgi?id=14642
    //assert(e.type.ty != Terror);    // FIXME
    if (e.type.ty == Terror)
        return ErrorExp.get();

    auto rgnpos = ctfeGlobals.region.savePos();

    import dmd.timetrace;
    timeTraceBeginEvent(TimeTraceEventType.ctfe);
    scope (exit) timeTraceEndEvent(TimeTraceEventType.ctfe, e);

    Expression result = interpret(e, null);

    // Report an error if the expression contained a `ThrowException` and
    // hence generated an uncaught exception
    if (auto tee = result.isThrownExceptionExp())
    {
        tee.generateUncaughtError();
        result = CTFEExp.cantexp;
    }
    else
        result = copyRegionExp(result);

    if (!CTFEExp.isCantExp(result))
        result = scrubReturnValue(e.loc, result);
    if (CTFEExp.isCantExp(result))
        result = ErrorExp.get();

    ctfeGlobals.region.release(rgnpos);

    return result;
}

/* Run CTFE on the expression, but allow the expression to be a TypeExp
 *  or a tuple containing a TypeExp. (This is required by pragma(msg)).
 */
public Expression ctfeInterpretForPragmaMsg(Expression e)
{
    if (e.op == EXP.error || e.op == EXP.type)
        return e;

    // It's also OK for it to be a function declaration (happens only with
    // __traits(getOverloads))
    if (auto ve = e.isVarExp())
        if (ve.var.isFuncDeclaration())
        {
            return e;
        }

    auto tup = e.isTupleExp();
    if (!tup)
        return e.ctfeInterpret();

    // Tuples need to be treated separately, since they are
    // allowed to contain a TypeExp in this case.

    Expressions* expsx = null;
    foreach (i, g; *tup.exps)
    {
        auto h = ctfeInterpretForPragmaMsg(g);
        if (h != g)
        {
            if (!expsx)
            {
                expsx = tup.exps.copy();
            }
            (*expsx)[i] = h;
        }
    }
    if (expsx)
    {
        auto te = new TupleExp(e.loc, expsx);
        expandTuples(te.exps);
        te.type = new TypeTuple(te.exps);
        return te;
    }
    return e;
}

public Expression getValue(VarDeclaration vd)
{
    return ctfeGlobals.stack.getValue(vd);
}

/*************************************************
 * Allocate an Expression in the ctfe region.
 * Params:
 *      T = type of Expression to allocate
 *      args = arguments to Expression's constructor
 * Returns:
 *      allocated Expression
 */
T ctfeEmplaceExp(T : Expression, Args...)(Args args)
{
    if (mem.isGCEnabled)
        return new T(args);
    auto p = ctfeGlobals.region.malloc(__traits(classInstanceSize, T));
    emplaceExp!T(p, args);
    return cast(T)p;
}

// CTFE diagnostic information
public extern (C++) void printCtfePerformanceStats()
{
    debug (SHOWPERFORMANCE)
    {
        printf("        ---- CTFE Performance ----\n");
        printf("max call depth = %d\tmax stack = %d\n", ctfeGlobals.maxCallDepth, ctfeGlobals.stack.maxStackUsage());
        printf("array allocs = %d\tassignments = %d\n\n", ctfeGlobals.numArrayAllocs, ctfeGlobals.numAssignments);
    }
}

/**************************
 */

void incArrayAllocs()
{
    ++ctfeGlobals.numArrayAllocs;
}

/* ================================================ Implementation ======================================= */

private:

/***************
 * Collect together globals used by CTFE
 */
struct CtfeGlobals
{
    Region region;

    CtfeStack stack;

    int callDepth = 0;        // current number of recursive calls

    // When printing a stack trace, suppress this number of calls
    int stackTraceCallsToSuppress = 0;

    int maxCallDepth = 0;     // highest number of recursive calls
    int numArrayAllocs = 0;   // Number of allocated arrays
    int numAssignments = 0;   // total number of assignments executed
}

__gshared CtfeGlobals ctfeGlobals;

enum CTFEGoal : int
{
    RValue,     /// Must return an Rvalue (== CTFE value)
    LValue,     /// Must return an Lvalue (== CTFE reference)
    Nothing,    /// The return value is not required
}

//debug = LOG;
//debug = LOGASSIGN;
//debug = LOGCOMPILE;
//debug = SHOWPERFORMANCE;

// Maximum allowable recursive function calls in CTFE
enum CTFE_RECURSION_LIMIT = 1000;

/**
 The values of all CTFE variables
 */
struct CtfeStack
{
private:
    /* The stack. Every declaration we encounter is pushed here,
     * together with the VarDeclaration, and the previous
     * stack address of that variable, so that we can restore it
     * when we leave the stack frame.
     * Note that when a function is forward referenced, the interpreter must
     * run semantic3, and that may start CTFE again with a NULL istate. Thus
     * the stack might not be empty when CTFE begins.
     *
     * Ctfe Stack addresses are just 0-based integers, but we save
     * them as 'void *' because Array can only do pointers.
     */
    Expressions values;         // values on the stack
    VarDeclarations vars;       // corresponding variables
    Array!(void*) savedId;      // id of the previous state of that var

    Array!(void*) frames;       // all previous frame pointers
    Expressions savedThis;      // all previous values of localThis

    /* Global constants get saved here after evaluation, so we never
     * have to redo them. This saves a lot of time and memory.
     */
    Expressions globalValues;   // values of global constants

    size_t framepointer;        // current frame pointer
    size_t maxStackPointer;     // most stack we've ever used
    Expression localThis;       // value of 'this', or NULL if none

public:
    size_t stackPointer() @safe
    {
        return values.length;
    }

    // The current value of 'this', or NULL if none
    Expression getThis() @safe
    {
        return localThis;
    }

    // Largest number of stack positions we've used
    size_t maxStackUsage() @safe
    {
        return maxStackPointer;
    }

    // Start a new stack frame, using the provided 'this'.
    void startFrame(Expression thisexp)
    {
        frames.push(cast(void*)cast(size_t)framepointer);
        savedThis.push(localThis);
        framepointer = stackPointer();
        localThis = thisexp;
    }

    void endFrame()
    {
        size_t oldframe = cast(size_t)frames[frames.length - 1];
        localThis = savedThis[savedThis.length - 1];
        popAll(framepointer);
        framepointer = oldframe;
        frames.setDim(frames.length - 1);
        savedThis.setDim(savedThis.length - 1);
    }

    bool isInCurrentFrame(VarDeclaration v)
    {
        if (v.isDataseg() && !v.isCTFE())
            return false; // It's a global
        return v.ctfeAdrOnStack >= framepointer;
    }

    Expression getValue(VarDeclaration v)
    {
        //printf("getValue() %s\n", v.toChars());
        if ((v.isDataseg() || v.storage_class & STC.manifest) && !v.isCTFE())
        {
            assert(v.ctfeAdrOnStack < globalValues.length);
            return globalValues[v.ctfeAdrOnStack];
        }
        assert(v.ctfeAdrOnStack < stackPointer());
        return values[v.ctfeAdrOnStack];
    }

    void setValue(VarDeclaration v, Expression e)
    {
        //printf("setValue() %s : %s\n", v.toChars(), e.toChars());
        assert(!v.isDataseg() || v.isCTFE());
        assert(v.ctfeAdrOnStack < stackPointer());
        values[v.ctfeAdrOnStack] = e;
    }

    void push(VarDeclaration v)
    {
        //printf("push() %s\n", v.toChars());
        assert(!v.isDataseg() || v.isCTFE());
        if (v.ctfeAdrOnStack != VarDeclaration.AdrOnStackNone && v.ctfeAdrOnStack >= framepointer)
        {
            // Already exists in this frame, reuse it.
            values[v.ctfeAdrOnStack] = null;
            return;
        }
        savedId.push(cast(void*)cast(size_t)v.ctfeAdrOnStack);
        v.ctfeAdrOnStack = cast(uint)values.length;
        vars.push(v);
        values.push(null);
    }

    void pop(VarDeclaration v)
    {
        assert(!v.isDataseg() || v.isCTFE());
        assert(!v.isReference());
        const oldid = v.ctfeAdrOnStack;
        v.ctfeAdrOnStack = cast(uint)cast(size_t)savedId[oldid];
        if (v.ctfeAdrOnStack == values.length - 1)
        {
            values.pop();
            vars.pop();
            savedId.pop();
        }
    }

    void popAll(size_t stackpointer)
    {
        if (stackPointer() > maxStackPointer)
            maxStackPointer = stackPointer();
        assert(values.length >= stackpointer);
        for (size_t i = stackpointer; i < values.length; ++i)
        {
            VarDeclaration v = vars[i];
            v.ctfeAdrOnStack = cast(uint)cast(size_t)savedId[i];
        }
        values.setDim(stackpointer);
        vars.setDim(stackpointer);
        savedId.setDim(stackpointer);
    }

    void saveGlobalConstant(VarDeclaration v, Expression e)
    {
        assert(v._init && (v.isConst() || v.isImmutable() || v.storage_class & STC.manifest) && !v.isCTFE());
        v.ctfeAdrOnStack = cast(uint)globalValues.length;
        globalValues.push(copyRegionExp(e));
    }
}

private struct InterState
{
    InterState* caller;     // calling function's InterState
    FuncDeclaration fd;     // function being interpreted
    Statement start;        // if !=NULL, start execution at this statement

    /* target of CTFEExp result; also
     * target of labelled CTFEExp or
     * CTFEExp. (null if no label).
     */
    Statement gotoTarget;
}

/*************************************
 * Attempt to interpret a function given the arguments.
 * Params:
 *      pue       = storage for result
 *      fd        = function being called
 *      istate    = state for calling function (NULL if none)
 *      arguments = function arguments
 *      thisarg   = 'this', if a needThis() function, NULL if not.
 *
 * Returns:
 * result expression if successful, EXP.cantExpression if not,
 * or CTFEExp if function returned void.
 */
private Expression interpretFunction(UnionExp* pue, FuncDeclaration fd, InterState* istate, Expressions* arguments, Expression thisarg)
{
    debug (LOG)
    {
        printf("\n********\n%s FuncDeclaration::interpret(istate = %p) %s\n", fd.loc.toChars(), istate, fd.toChars());
    }

    scope dlg = () {
        import dmd.common.outbuffer;
        auto strbuf = OutBuffer(20);
        strbuf.writestring(fd.toPrettyChars());
        strbuf.write("(");
        if (arguments)
        {
            foreach (i, arg; *arguments)
            {
                if (i > 0)
                    strbuf.write(", ");
                strbuf.writestring(arg.toChars());
            }
        }
        strbuf.write(")");
        return strbuf.extractSlice();
    };
    import dmd.timetrace;
    timeTraceBeginEvent(TimeTraceEventType.ctfeCall);
    scope (exit) timeTraceEndEvent(TimeTraceEventType.ctfeCall, fd, dlg);

    void fdError(const(char)* msg)
    {
        error(fd.loc, "%s `%s` %s", fd.kind, fd.toPrettyChars, msg);
    }

    assert(pue);
    if (fd.semanticRun == PASS.semantic3)
    {
        fdError("circular dependency. Functions cannot be interpreted while being compiled");
        return CTFEExp.cantexp;
    }
    if (!functionSemantic3(fd))
        return CTFEExp.cantexp;
    if (fd.semanticRun < PASS.semantic3done)
    {
        fdError("circular dependency. Functions cannot be interpreted while being compiled");
        return CTFEExp.cantexp;
    }

    auto tf = fd.type.toBasetype().isTypeFunction();
    if (tf.parameterList.varargs != VarArg.none && arguments &&
        ((fd.parameters && arguments.length != fd.parameters.length) || (!fd.parameters && arguments.length)))
    {
        fdError("C-style variadic functions are not yet implemented in CTFE");
        return CTFEExp.cantexp;
    }

    // Nested functions always inherit the 'this' pointer from the parent,
    // except for delegates. (Note that the 'this' pointer may be null).
    // Func literals report isNested() even if they are in global scope,
    // so we need to check that the parent is a function.
    if (fd.isNested() && fd.toParentLocal().isFuncDeclaration() && !thisarg && istate)
        thisarg = ctfeGlobals.stack.getThis();

    if (fd.needThis() && !thisarg)
    {
        // error, no this. Prevent segfault.
        // Here should be unreachable by the strict 'this' check in front-end.
        error(fd.loc, "%s `%s` need `this` to access member `%s`", fd.kind, fd.toPrettyChars, fd.toChars());
        return CTFEExp.cantexp;
    }

    // Place to hold all the arguments to the function while
    // we are evaluating them.
    size_t dim = arguments ? arguments.length : 0;
    assert((fd.parameters ? fd.parameters.length : 0) == dim);

    /* Evaluate all the arguments to the function,
     * store the results in eargs[]
     */
    Expressions eargs = Expressions(dim);
    for (size_t i = 0; i < dim; i++)
    {
        Expression earg = (*arguments)[i];
        Parameter fparam = tf.parameterList[i];

        if (fparam.isReference())
        {
            if (!istate && (fparam.storageClass & STC.out_))
            {
                // initializing an out parameter involves writing to it.
                error(earg.loc, "global `%s` cannot be passed as an `out` parameter at compile time", earg.toChars());
                return CTFEExp.cantexp;
            }
            // Convert all reference arguments into lvalue references
            earg = interpretRegion(earg, istate, CTFEGoal.LValue);
            if (CTFEExp.isCantExp(earg))
                return earg;
        }
        else if (fparam.isLazy())
        {
        }
        else
        {
            /* Value parameters
             */
            Type ta = fparam.type.toBasetype();
            if (ta.ty == Tsarray)
                if (auto eaddr = earg.isAddrExp())
                {
                    /* Static arrays are passed by a simple pointer.
                     * Skip past this to get at the actual arg.
                     */
                    earg = eaddr.e1;
                }

            earg = interpretRegion(earg, istate);
            if (CTFEExp.isCantExp(earg))
                return earg;

            /* Struct literals are passed by value, but we don't need to
             * copy them if they are passed as const
             */
            if (earg.op == EXP.structLiteral && !(fparam.storageClass & (STC.const_ | STC.immutable_)))
                earg = copyLiteral(earg).copy();
        }
        if (auto tee = earg.isThrownExceptionExp())
        {
            if (istate)
                return tee;
            tee.generateUncaughtError();
            return CTFEExp.cantexp;
        }
        eargs[i] = earg;
    }

    // Now that we've evaluated all the arguments, we can start the frame
    // (this is the moment when the 'call' actually takes place).
    InterState istatex;
    istatex.caller = istate;
    istatex.fd = fd;

    if (fd.hasDualContext())
    {
        Expression arg0 = thisarg;
        if (arg0 && arg0.type.ty == Tstruct)
        {
            Type t = arg0.type.pointerTo();
            arg0 = ctfeEmplaceExp!AddrExp(arg0.loc, arg0);
            arg0.type = t;
        }
        auto elements = new Expressions(2);
        (*elements)[0] = arg0;
        (*elements)[1] = ctfeGlobals.stack.getThis();
        Type t2 = Type.tvoidptr.sarrayOf(2);
        const loc = thisarg ? thisarg.loc : fd.loc;
        thisarg = ctfeEmplaceExp!ArrayLiteralExp(loc, t2, elements);
        thisarg = ctfeEmplaceExp!AddrExp(loc, thisarg);
        thisarg.type = t2.pointerTo();
    }

    ctfeGlobals.stack.startFrame(thisarg);
    if (fd.vthis && thisarg)
    {
        ctfeGlobals.stack.push(fd.vthis);
        setValue(fd.vthis, thisarg);
    }

    for (size_t i = 0; i < dim; i++)
    {
        Expression earg = eargs[i];
        Parameter fparam = tf.parameterList[i];
        VarDeclaration v = (*fd.parameters)[i];
        debug (LOG)
        {
            printf("arg[%zu] = %s\n", i, earg.toChars());
        }
        ctfeGlobals.stack.push(v);

        if (fparam.isReference() && earg.op == EXP.variable &&
            earg.isVarExp().var.toParent2() == fd)
        {
            VarDeclaration vx = earg.isVarExp().var.isVarDeclaration();
            if (!vx)
            {
                error(fd.loc, "%s `%s` cannot interpret `%s` as a `ref` parameter", fd.kind, fd.toPrettyChars, earg.toChars());
                return CTFEExp.cantexp;
            }

            /* vx is a variable that is declared in fd.
             * It means that fd is recursively called. e.g.
             *
             *  void fd(int n, ref int v = dummy) {
             *      int vx;
             *      if (n == 1) fd(2, vx);
             *  }
             *  fd(1);
             *
             * The old value of vx on the stack in fd(1)
             * should be saved at the start of fd(2, vx) call.
             */
            const oldadr = vx.ctfeAdrOnStack;

            ctfeGlobals.stack.push(vx);
            assert(!hasValue(vx)); // vx is made uninitialized

            // https://issues.dlang.org/show_bug.cgi?id=14299
            // v.ctfeAdrOnStack should be saved already
            // in the stack before the overwrite.
            v.ctfeAdrOnStack = oldadr;
            assert(hasValue(v)); // ref parameter v should refer existing value.
        }
        else
        {
            // Value parameters and non-trivial references
            setValueWithoutChecking(v, earg);
        }
        debug (LOG)
        {
            printf("interpreted arg[%zu] = %s\n", i, earg.toChars());
            showCtfeExpr(earg);
        }
        debug (LOGASSIGN)
        {
            printf("interpreted arg[%zu] = %s\n", i, earg.toChars());
            showCtfeExpr(earg);
        }
    }

    if (fd.vresult)
        ctfeGlobals.stack.push(fd.vresult);

    // Enter the function
    ++ctfeGlobals.callDepth;
    if (ctfeGlobals.callDepth > ctfeGlobals.maxCallDepth)
        ctfeGlobals.maxCallDepth = ctfeGlobals.callDepth;

    Expression e = null;
    while (1)
    {
        if (ctfeGlobals.callDepth > CTFE_RECURSION_LIMIT)
        {
            fdError("CTFE recursion limit exceeded");
            e = CTFEExp.cantexp;
            break;
        }
        e = interpretStatement(pue, fd.fbody, &istatex);
        if (CTFEExp.isCantExp(e))
        {
            debug (LOG)
            {
                printf("function body failed to interpret\n");
            }
        }

        if (istatex.start)
        {
            error(fd.loc, "%s `%s` CTFE internal error: failed to resume at statement `%s`", fd.kind, fd.toPrettyChars, istatex.start.toChars());
            return CTFEExp.cantexp;
        }

        /* This is how we deal with a recursive statement AST
         * that has arbitrary goto statements in it.
         * Bubble up a 'result' which is the target of the goto
         * statement, then go recursively down the AST looking
         * for that statement, then execute starting there.
         */
        if (CTFEExp.isGotoExp(e))
        {
            istatex.start = istatex.gotoTarget; // set starting statement
            istatex.gotoTarget = null;
        }
        else
        {
            assert(!e || (e.op != EXP.continue_ && e.op != EXP.break_));
            break;
        }
    }
    // If fell off the end of a void function, return void
    if (!e)
    {
        if (tf.next.ty == Tvoid)
            e = CTFEExp.voidexp;
        else
        {
            /* missing a return statement can happen with C functions
             * https://issues.dlang.org/show_bug.cgi?id=23056
             */
            fdError("no return value from function");
            e = CTFEExp.cantexp;
        }
    }

    if (tf.isRef && e.op == EXP.variable && e.isVarExp().var == fd.vthis)
        e = thisarg;
    if (tf.isRef && fd.hasDualContext() && e.op == EXP.index)
    {
        auto ie = e.isIndexExp();
        auto pe = ie.e1.isPtrExp();
        auto ve = !pe ?  null : pe.e1.isVarExp();
        if (ve && ve.var == fd.vthis)
        {
            auto ne = ie.e2.isIntegerExp();
            assert(ne);
            auto ale = thisarg.isAddrExp().e1.isArrayLiteralExp();
            e = (*ale.elements)[cast(size_t)ne.getInteger()];
            if (auto ae = e.isAddrExp())
            {
                e = ae.e1;
            }
        }
    }

    // Leave the function
    --ctfeGlobals.callDepth;

    ctfeGlobals.stack.endFrame();

    // If it generated an uncaught exception, report error.
    if (!istate && e.isThrownExceptionExp())
    {
        if (e == pue.exp())
            e = pue.copy();
        e.isThrownExceptionExp().generateUncaughtError();
        e = CTFEExp.cantexp;
    }

    return e;
}

/// used to collect coverage information in ctfe
void incUsageCtfe(InterState* istate, Loc loc)
{
    if (global.params.ctfe_cov && istate)
    {
        auto line = loc.linnum;
        auto mod = istate.fd.getModule();

        ++mod.ctfe_cov[line];
    }
}

/***********************************
 * Interpret the statement.
 * Params:
 *    s = Statement to interpret
 *    istate = context
 * Returns:
 *      NULL    continue to next statement
 *      EXP.cantExpression      cannot interpret statement at compile time
 *      !NULL   expression from return statement, or thrown exception
 */

Expression interpretStatement(Statement s, InterState* istate)
{
    UnionExp ue = void;
    auto result = interpretStatement(&ue, s, istate);
    if (result == ue.exp())
        result = ue.copy();
    return result;
}

///
Expression interpretStatement(UnionExp* pue, Statement s, InterState* istate)
{
    Expression result;

    // If e is EXP.throw_exception or EXP.cantExpression,
    // set it to 'result' and returns true.
    bool exceptionOrCant(Expression e)
    {
        if (exceptionOrCantInterpret(e))
        {
            // Make sure e is not pointing to a stack temporary
            result = (e.op == EXP.cantExpression) ? CTFEExp.cantexp : e;
            return true;
        }
        return false;
    }

    /******************************** Statement ***************************/

    void visitDefaultCase(Statement s)
    {
        debug (LOG)
        {
            printf("%s Statement::interpret() %s\n", s.loc.toChars(), s.toChars());
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }

        error(s.loc, "statement `%s` cannot be interpreted at compile time", s.toChars());
        result = CTFEExp.cantexp;
    }

    void visitExp(ExpStatement s)
    {
        debug (LOG)
        {
            printf("%s ExpStatement::interpret(%s)\n", s.loc.toChars(), s.exp ? s.exp.toChars() : "");
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }
        if (s.exp && s.exp.hasCode)
            incUsageCtfe(istate, s.loc);

        Expression e = interpret(pue, s.exp, istate, CTFEGoal.Nothing);
        if (exceptionOrCant(e))
            return;
    }

    void visitDtorExp(DtorExpStatement s)
    {
        visitExp(s);
    }

    void visitCompound(CompoundStatement s)
    {
        debug (LOG)
        {
            printf("%s CompoundStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;

        const dim = s.statements ? s.statements.length : 0;
        foreach (i; 0 .. dim)
        {
            Statement sx = (*s.statements)[i];
            result = interpretStatement(pue, sx, istate);
            if (result)
                break;
        }
        debug (LOG)
        {
            printf("%s -CompoundStatement::interpret() %p\n", s.loc.toChars(), result);
        }
    }

    void visitCompoundAsm(CompoundAsmStatement s)
    {
        visitCompound(s);
    }

    void visitUnrolledLoop(UnrolledLoopStatement s)
    {
        debug (LOG)
        {
            printf("%s UnrolledLoopStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;

        const dim = s.statements ? s.statements.length : 0;
        foreach (i; 0 .. dim)
        {
            Statement sx = (*s.statements)[i];
            Expression e = interpretStatement(pue, sx, istate);
            if (!e) // succeeds to interpret, or goto target was not found
                continue;
            if (exceptionOrCant(e))
                return;
            if (e.op == EXP.break_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // break at a higher level
                    return;
                }
                istate.gotoTarget = null;
                result = null;
                return;
            }
            if (e.op == EXP.continue_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // continue at a higher level
                    return;
                }
                istate.gotoTarget = null;
                continue;
            }

            // expression from return statement, or thrown exception
            result = e;
            break;
        }
    }

    void visitIf(IfStatement s)
    {
        debug (LOG)
        {
            printf("%s IfStatement::interpret(%s)\n", s.loc.toChars(), s.condition.toChars());
        }
        incUsageCtfe(istate, s.loc);
        if (istate.start == s)
            istate.start = null;
        if (istate.start)
        {
            Expression e = null;
            e = interpretStatement(s.ifbody, istate);
            if (!e && istate.start)
                e = interpretStatement(s.elsebody, istate);
            result = e;
            return;
        }

        UnionExp ue = void;
        Expression e = interpret(&ue, s.condition, istate);
        assert(e);
        if (exceptionOrCant(e))
            return;

        if (isTrueBool(e))
            result = interpretStatement(pue, s.ifbody, istate);
        else if (e.toBool().hasValue(false))
            result = interpretStatement(pue, s.elsebody, istate);
        else
        {
            // no error, or assert(0)?
            result = CTFEExp.cantexp;
        }
    }

    void visitScope(ScopeStatement s)
    {
        debug (LOG)
        {
            printf("%s ScopeStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;

        result = interpretStatement(pue, s.statement, istate);
    }

    void visitReturn(ReturnStatement s)
    {
        debug (LOG)
        {
            printf("%s ReturnStatement::interpret(%s)\n", s.loc.toChars(), s.exp ? s.exp.toChars() : "");
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }

        if (!s.exp)
        {
            result = CTFEExp.voidexp;
            return;
        }

        incUsageCtfe(istate, s.loc);
        assert(istate && istate.fd && istate.fd.type && istate.fd.type.ty == Tfunction);
        TypeFunction tf = cast(TypeFunction)istate.fd.type;

        /* If the function returns a ref AND it's been called from an assignment,
         * we need to return an lvalue. Otherwise, just do an (rvalue) interpret.
         */
        if (tf.isRef)
        {
            result = interpret(pue, s.exp, istate, CTFEGoal.LValue);
            return;
        }
        if (tf.next && tf.next.ty == Tdelegate && istate.fd.closureVars.length > 0)
        {
            // To support this, we need to copy all the closure vars
            // into the delegate literal.
            error(s.loc, "closures are not yet supported in CTFE");
            result = CTFEExp.cantexp;
            return;
        }

        // We need to treat pointers specially, because EXP.symbolOffset can be used to
        // return a value OR a pointer
        Expression e = interpret(pue, s.exp, istate);
        if (exceptionOrCant(e))
            return;

        /**
         * Interpret `return a ~= b` (i.e. `return _d_arrayappendT{,Trace}(a, b)`) as:
         *     a ~= b;
         *     return a;
         * This is needed because `a ~= b` has to be interpreted as an lvalue, in order to avoid
         * assigning a larger array into a smaller one, such as:
         *    `a = [1, 2], a ~= [3]` => `[1, 2] ~= [3]` => `[1, 2] = [1, 2, 3]`
         */
        if (isRuntimeHook(s.exp, Id._d_arrayappendT) || isRuntimeHook(s.exp, Id._d_arrayappendTTrace))
        {
            auto rs = new ReturnStatement(s.loc, e);
            visitReturn(rs);
            return;
        }

        // Disallow returning pointers to stack-allocated variables (bug 7876)
        if (!stopPointersEscaping(s.loc, e))
        {
            result = CTFEExp.cantexp;
            return;
        }

        if (needToCopyLiteral(e))
            e = copyLiteral(e).copy();
        debug (LOGASSIGN)
        {
            printf("RETURN %s\n", s.loc.toChars());
            showCtfeExpr(e);
        }
        result = e;
    }

    void visitBreak(BreakStatement s)
    {
        debug (LOG)
        {
            printf("%s BreakStatement::interpret()\n", s.loc.toChars());
        }
        incUsageCtfe(istate, s.loc);
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }

        istate.gotoTarget = findGotoTarget(istate, s.ident);
        result = CTFEExp.breakexp;
    }

    void visitContinue(ContinueStatement s)
    {
        debug (LOG)
        {
            printf("%s ContinueStatement::interpret()\n", s.loc.toChars());
        }
        incUsageCtfe(istate, s.loc);
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }

        istate.gotoTarget = findGotoTarget(istate, s.ident);
        result = CTFEExp.continueexp;
    }

    void visitWhile(WhileStatement s)
    {
        debug (LOG)
        {
            printf("WhileStatement::interpret()\n");
        }
        assert(0); // rewritten to ForStatement
    }

    void visitDo(DoStatement s)
    {
        debug (LOG)
        {
            printf("%s DoStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;

        while (1)
        {
            Expression e = interpretStatement(s._body, istate);
            if (!e && istate.start) // goto target was not found
                return;
            assert(!istate.start);

            if (exceptionOrCant(e))
                return;
            if (e && e.op == EXP.break_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // break at a higher level
                    return;
                }
                istate.gotoTarget = null;
                break;
            }
            if (e && e.op == EXP.continue_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // continue at a higher level
                    return;
                }
                istate.gotoTarget = null;
                e = null;
            }
            if (e)
            {
                result = e; // bubbled up from ReturnStatement
                return;
            }

            UnionExp ue = void;
            incUsageCtfe(istate, s.condition.loc);
            e = interpret(&ue, s.condition, istate);
            if (exceptionOrCant(e))
                return;
            if (!e.isConst())
            {
                result = CTFEExp.cantexp;
                return;
            }
            if (e.toBool().hasValue(false))
                break;
            assert(isTrueBool(e));
        }
        assert(result is null);
    }

    void visitFor(ForStatement s)
    {
        debug (LOG)
        {
            printf("%s ForStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;

        UnionExp ueinit = void;
        Expression ei = interpretStatement(&ueinit, s._init, istate);
        if (exceptionOrCant(ei))
            return;
        assert(!ei); // s.init never returns from function, or jumps out from it

        while (1)
        {
            if (s.condition && !istate.start)
            {
                UnionExp ue = void;
                incUsageCtfe(istate, s.condition.loc);
                Expression e = interpret(&ue, s.condition, istate);
                if (exceptionOrCant(e))
                    return;
                if (e.toBool().hasValue(false))
                    break;
                assert(isTrueBool(e));
            }

            Expression e = interpretStatement(pue, s._body, istate);
            if (!e && istate.start) // goto target was not found
                return;
            assert(!istate.start);

            if (exceptionOrCant(e))
                return;
            if (e && e.op == EXP.break_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // break at a higher level
                    return;
                }
                istate.gotoTarget = null;
                break;
            }
            if (e && e.op == EXP.continue_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // continue at a higher level
                    return;
                }
                istate.gotoTarget = null;
                e = null;
            }
            if (e)
            {
                result = e; // bubbled up from ReturnStatement
                return;
            }

            UnionExp uei = void;
            if (s.increment)
                incUsageCtfe(istate, s.increment.loc);
            e = interpret(&uei, s.increment, istate, CTFEGoal.Nothing);
            if (exceptionOrCant(e))
                return;
        }
        assert(result is null);
    }

    void visitForeach(ForeachStatement s)
    {
        assert(0); // rewritten to ForStatement
    }

    void visitForeachRange(ForeachRangeStatement s)
    {
        assert(0); // rewritten to ForStatement
    }

    void visitSwitch(SwitchStatement s)
    {
        debug (LOG)
        {
            printf("%s SwitchStatement::interpret()\n", s.loc.toChars());
        }
        incUsageCtfe(istate, s.loc);
        if (istate.start == s)
            istate.start = null;
        if (istate.start)
        {
            Expression e = interpretStatement(s._body, istate);
            if (istate.start) // goto target was not found
                return;
            if (exceptionOrCant(e))
                return;
            if (e && e.op == EXP.break_)
            {
                if (istate.gotoTarget && istate.gotoTarget != s)
                {
                    result = e; // break at a higher level
                    return;
                }
                istate.gotoTarget = null;
                e = null;
            }
            result = e;
            return;
        }

        UnionExp uecond = void;
        Expression econdition = interpret(&uecond, s.condition, istate);
        if (exceptionOrCant(econdition))
            return;

        Statement scase = null;
        if (s.cases)
            foreach (cs; *s.cases)
            {
                UnionExp uecase = void;
                Expression ecase = interpret(&uecase, cs.exp, istate);
                if (exceptionOrCant(ecase))
                    return;
                if (ctfeEqual(cs.exp.loc, EXP.equal, econdition, ecase))
                {
                    scase = cs;
                    break;
                }
            }
        if (!scase)
        {
            if (!s.hasDefault)
                error(s.loc, "no `default` or `case` for `%s` in `switch` statement", econdition.toChars());
            scase = s.sdefault;
        }

        assert(scase);

        /* Jump to scase
         */
        istate.start = scase;
        Expression e = interpretStatement(pue, s._body, istate);
        assert(!istate.start); // jump must not fail
        if (e && e.op == EXP.break_)
        {
            if (istate.gotoTarget && istate.gotoTarget != s)
            {
                result = e; // break at a higher level
                return;
            }
            istate.gotoTarget = null;
            e = null;
        }
        result = e;
    }

    void visitCase(CaseStatement s)
    {
        debug (LOG)
        {
            printf("%s CaseStatement::interpret(%s) this = %p\n", s.loc.toChars(), s.exp.toChars(), s);
        }
        incUsageCtfe(istate, s.loc);
        if (istate.start == s)
            istate.start = null;

        result = interpretStatement(pue, s.statement, istate);
    }

    void visitDefault(DefaultStatement s)
    {
        debug (LOG)
        {
            printf("%s DefaultStatement::interpret()\n", s.loc.toChars());
        }
        incUsageCtfe(istate, s.loc);
        if (istate.start == s)
            istate.start = null;

        result = interpretStatement(pue, s.statement, istate);
    }

    void visitGoto(GotoStatement s)
    {
        debug (LOG)
        {
            printf("%s GotoStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }
        incUsageCtfe(istate, s.loc);

        assert(s.label && s.label.statement);
        istate.gotoTarget = s.label.statement;
        result = CTFEExp.gotoexp;
    }

    void visitGotoCase(GotoCaseStatement s)
    {
        debug (LOG)
        {
            printf("%s GotoCaseStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }
        incUsageCtfe(istate, s.loc);

        assert(s.cs);
        istate.gotoTarget = s.cs;
        result = CTFEExp.gotoexp;
    }

    void visitGotoDefault(GotoDefaultStatement s)
    {
        debug (LOG)
        {
            printf("%s GotoDefaultStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }
        incUsageCtfe(istate, s.loc);

        assert(s.sw && s.sw.sdefault);
        istate.gotoTarget = s.sw.sdefault;
        result = CTFEExp.gotoexp;
    }

    void visitLabel(LabelStatement s)
    {
        debug (LOG)
        {
            printf("%s LabelStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;

        result = interpretStatement(pue, s.statement, istate);
    }

    void visitTryCatch(TryCatchStatement s)
    {
        debug (LOG)
        {
            printf("%s TryCatchStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;
        if (istate.start)
        {
            Expression e = null;
            e = interpretStatement(pue, s._body, istate);
            foreach (ca; *s.catches)
            {
                if (e || !istate.start) // goto target was found
                    break;
                e = interpretStatement(pue, ca.handler, istate);
            }
            result = e;
            return;
        }

        Expression e = interpretStatement(s._body, istate);

        // An exception was thrown
        if (e && e.isThrownExceptionExp())
        {
            ThrownExceptionExp ex = e.isThrownExceptionExp();
            Type extype = ex.thrown.originalClass().type;

            // Search for an appropriate catch clause.
            foreach (ca; *s.catches)
            {
                Type catype = ca.type;
                import dmd.typesem : isBaseOf;
                if (!catype.equals(extype) && !catype.isBaseOf(extype, null))
                    continue;

                // Execute the handler
                if (ca.var)
                {
                    ctfeGlobals.stack.push(ca.var);
                    setValue(ca.var, ex.thrown);
                }
                e = interpretStatement(ca.handler, istate);
                while (CTFEExp.isGotoExp(e))
                {
                    /* This is an optimization that relies on the locality of the jump target.
                     * If the label is in the same catch handler, the following scan
                     * would find it quickly and can reduce jump cost.
                     * Otherwise, the catch block may be unnnecessary scanned again
                     * so it would make CTFE speed slower.
                     */
                    InterState istatex = *istate;
                    istatex.start = istate.gotoTarget; // set starting statement
                    istatex.gotoTarget = null;
                    Expression eh = interpretStatement(ca.handler, &istatex);
                    if (istatex.start)
                    {
                        // The goto target is outside the current scope.
                        break;
                    }
                    // The goto target was within the body.
                    if (CTFEExp.isCantExp(eh))
                    {
                        e = eh;
                        break;
                    }
                    *istate = istatex;
                    e = eh;
                }
                break;
            }
        }
        result = e;
    }

    void visitTryFinally(TryFinallyStatement s)
    {
        debug (LOG)
        {
            printf("%s TryFinallyStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;
        if (istate.start)
        {
            Expression e = null;
            e = interpretStatement(pue, s._body, istate);
            // Jump into/out from finalbody is disabled in semantic analysis.
            // and jump inside will be handled by the ScopeStatement == finalbody.
            result = e;
            return;
        }

        Expression ex = interpretStatement(s._body, istate);
        if (CTFEExp.isCantExp(ex))
        {
            result = ex;
            return;
        }
        while (CTFEExp.isGotoExp(ex))
        {
            // If the goto target is within the body, we must not interpret the finally statement,
            // because that will call destructors for objects within the scope, which we should not do.
            InterState istatex = *istate;
            istatex.start = istate.gotoTarget; // set starting statement
            istatex.gotoTarget = null;
            Expression bex = interpretStatement(s._body, &istatex);
            if (istatex.start)
            {
                // The goto target is outside the current scope.
                break;
            }
            // The goto target was within the body.
            if (CTFEExp.isCantExp(bex))
            {
                result = bex;
                return;
            }
            *istate = istatex;
            ex = bex;
        }

        Expression ey = interpretStatement(s.finalbody, istate);
        if (CTFEExp.isCantExp(ey))
        {
            result = ey;
            return;
        }
        if (ey && ey.isThrownExceptionExp())
        {
            // Check for collided exceptions
            if (ex && ex.isThrownExceptionExp())
                ex = chainExceptions(ex.isThrownExceptionExp(), ey.isThrownExceptionExp());
            else
                ex = ey;
        }
        result = ex;
    }

    void visitThrow(ThrowStatement s)
    {
        debug (LOG)
        {
            printf("%s ThrowStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }

        interpretThrow(result, s.exp, s.loc, istate);
    }

    void visitScopeGuard(ScopeGuardStatement s)
    {
        assert(0);
    }

    void visitWith(WithStatement s)
    {
        debug (LOG)
        {
            printf("%s WithStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start == s)
            istate.start = null;
        if (istate.start)
        {
            result = s._body ? interpretStatement(s._body, istate) : null;
            return;
        }

        // If it is with(Enum) {...}, just execute the body.
        if (s.exp.op == EXP.scope_ || s.exp.op == EXP.type)
        {
            result = interpretStatement(pue, s._body, istate);
            return;
        }

        incUsageCtfe(istate, s.loc);

        Expression e = interpret(s.exp, istate);
        if (exceptionOrCant(e))
            return;

        if (s.wthis.type.ty == Tpointer && s.exp.type.ty != Tpointer)
        {
            e = ctfeEmplaceExp!AddrExp(s.loc, e, s.wthis.type);
        }
        ctfeGlobals.stack.push(s.wthis);
        setValue(s.wthis, e);
        e = interpretStatement(s._body, istate);
        while (CTFEExp.isGotoExp(e))
        {
            /* This is an optimization that relies on the locality of the jump target.
             * If the label is in the same WithStatement, the following scan
             * would find it quickly and can reduce jump cost.
             * Otherwise, the statement body may be unnnecessary scanned again
             * so it would make CTFE speed slower.
             */
            InterState istatex = *istate;
            istatex.start = istate.gotoTarget; // set starting statement
            istatex.gotoTarget = null;
            Expression ex = interpretStatement(s._body, &istatex);
            if (istatex.start)
            {
                // The goto target is outside the current scope.
                break;
            }
            // The goto target was within the body.
            if (CTFEExp.isCantExp(ex))
            {
                e = ex;
                break;
            }
            *istate = istatex;
            e = ex;
        }
        ctfeGlobals.stack.pop(s.wthis);
        result = e;
    }

    void visitAsm(AsmStatement s)
    {
        debug (LOG)
        {
            printf("%s AsmStatement::interpret()\n", s.loc.toChars());
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }
        error(s.loc, "`asm` statements cannot be interpreted at compile time");
        result = CTFEExp.cantexp;
    }

    void visitInlineAsm(InlineAsmStatement s)
    {
        visitAsm(s);
    }

    void visitGccAsm(GccAsmStatement s)
    {
        visitAsm(s);
    }

    void visitImport(ImportStatement s)
    {
        debug (LOG)
        {
            printf("ImportStatement::interpret()\n");
        }
        if (istate.start)
        {
            if (istate.start != s)
                return;
            istate.start = null;
        }
    }

    if (!s)
        return null;

    mixin VisitStatement!void visit;
    visit.VisitStatement(s);
    return result;
}

///

private extern (C++) final class Interpreter : Visitor
{
    alias visit = Visitor.visit;
public:
    InterState* istate;
    CTFEGoal goal;
    Expression result;
    UnionExp* pue;              // storage for `result`

    extern (D) this(UnionExp* pue, InterState* istate, CTFEGoal goal) scope @safe
    {
        this.pue = pue;
        this.istate = istate;
        this.goal = goal;
    }

    // If e is EXP.throw_exception or EXP.cantExpression,
    // set it to 'result' and returns true.
    bool exceptionOrCant(Expression e)
    {
        if (exceptionOrCantInterpret(e))
        {
            // Make sure e is not pointing to a stack temporary
            result = (e.op == EXP.cantExpression) ? CTFEExp.cantexp : e;
            return true;
        }
        return false;
    }

    /******************************** Expression ***************************/

    override void visit(Expression e)
    {
        debug (LOG)
        {
            printf("%s Expression::interpret() '%s' %s\n", e.loc.toChars(), EXPtoString(e.op).ptr, e.toChars());
            printf("type = %s\n", e.type.toChars());
            showCtfeExpr(e);
        }
        error(e.loc, "cannot interpret `%s` at compile time", e.toChars());
        result = CTFEExp.cantexp;
    }

    override void visit(TypeExp e)
    {
        debug (LOG)
        {
            printf("%s TypeExp.interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        result = e;
    }

    override void visit(ThisExp e)
    {
        debug (LOG)
        {
            printf("%s ThisExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (goal == CTFEGoal.LValue)
        {
            // We might end up here with istate being zero
            // https://issues.dlang.org/show_bug.cgi?id=16382
            if (istate && istate.fd.vthis)
            {
                result = ctfeEmplaceExp!VarExp(e.loc, istate.fd.vthis);
                if (istate.fd.hasDualContext())
                {
                    result = ctfeEmplaceExp!PtrExp(e.loc, result);
                    result.type = Type.tvoidptr.sarrayOf(2);
                    result = ctfeEmplaceExp!IndexExp(e.loc, result, IntegerExp.literal!0);
                }
                result.type = e.type;
            }
            else
                result = e;
            return;
        }

        result = ctfeGlobals.stack.getThis();
        if (result)
        {
            if (istate && istate.fd.hasDualContext())
            {
                assert(result.op == EXP.address);
                result = result.isAddrExp().e1;
                assert(result.op == EXP.arrayLiteral);
                result = (*result.isArrayLiteralExp().elements)[0];
                if (e.type.ty == Tstruct)
                {
                    result = result.isAddrExp().e1;
                }
                return;
            }
            assert(result.op == EXP.structLiteral || result.op == EXP.classReference || result.op == EXP.type);
            return;
        }
        error(e.loc, "value of `this` is not known at compile time");
        result = CTFEExp.cantexp;
    }

    override void visit(NullExp e)
    {
        result = e;
    }

    override void visit(IntegerExp e)
    {
        debug (LOG)
        {
            printf("%s IntegerExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        result = e;
    }

    override void visit(RealExp e)
    {
        debug (LOG)
        {
            printf("%s RealExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        result = e;
    }

    override void visit(ComplexExp e)
    {
        result = e;
    }

    override void visit(StringExp e)
    {
        debug (LOG)
        {
            printf("%s StringExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.ownedByCtfe >= OwnedBy.ctfe) // We've already interpreted the string
        {
            result = e;
            return;
        }

        if (e.type.ty != Tsarray ||
            (cast(TypeNext)e.type).next.mod & (MODFlags.const_ | MODFlags.immutable_))
        {
            // If it's immutable, we don't need to dup it. Attempts to modify
            // string literals are prevented in BinExp::interpretAssignCommon.
            result = e;
        }
        else
        {
            // https://issues.dlang.org/show_bug.cgi?id=20811
            // Create a copy of mutable string literals, so that any change in
            // value via an index or slice will not survive CTFE.
            *pue = copyLiteral(e);
            result = pue.exp();
        }
    }

    override void visit(FuncExp e)
    {
        debug (LOG)
        {
            printf("%s FuncExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        result = e;
    }

    override void visit(SymOffExp e)
    {
        debug (LOG)
        {
            printf("%s SymOffExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.var.isFuncDeclaration() && e.offset == 0)
        {
            result = e;
            return;
        }
        if (isTypeInfo_Class(e.type) && e.offset == 0)
        {
            result = e;
            return;
        }
        if (e.type.ty != Tpointer)
        {
            // Probably impossible
            error(e.loc, "cannot interpret `%s` at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        Type pointee = (cast(TypePointer)e.type).next;
        if (e.var.isThreadlocal())
        {
            error(e.loc, "cannot take address of thread-local variable %s at compile time", e.var.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        // Check for taking an address of a shared variable.
        // If the shared variable is an array, the offset might not be zero.
        Type fromType = null;
        if (e.var.type.isStaticOrDynamicArray())
        {
            fromType = (cast(TypeArray)e.var.type).next;
        }
        if (e.var.isDataseg() && ((e.offset == 0 && isSafePointerCast(e.var.type, pointee)) ||
                                  (fromType && isSafePointerCast(fromType, pointee)) ||
                                  (e.var.isCsymbol() && e.offset + pointee.size() <= e.var.type.size())))
        {
            result = e;
            return;
        }

        Expression val = getVarExp(e.loc, istate, e.var, goal);
        if (exceptionOrCant(val))
            return;
        if (val.type.isStaticOrDynamicArray())
        {
            // Check for unsupported type painting operations
            Type elemtype = (cast(TypeArray)val.type).next;
            const elemsize = elemtype.size();

            // It's OK to cast from fixed length to fixed length array, eg &int[n] to int[d]*.
            if (val.type.ty == Tsarray && pointee.ty == Tsarray && elemsize == pointee.nextOf().size())
            {
                size_t d = cast(size_t)(cast(TypeSArray)pointee).dim.toInteger();
                Expression elwr = ctfeEmplaceExp!IntegerExp(e.loc, e.offset / elemsize, Type.tsize_t);
                Expression eupr = ctfeEmplaceExp!IntegerExp(e.loc, e.offset / elemsize + d, Type.tsize_t);

                // Create a CTFE pointer &val[ofs..ofs+d]
                auto se = ctfeEmplaceExp!SliceExp(e.loc, val, elwr, eupr);
                se.type = pointee;
                emplaceExp!(AddrExp)(pue, e.loc, se, e.type);
                result = pue.exp();
                return;
            }

            if (!isSafePointerCast(elemtype, pointee))
            {
                // It's also OK to cast from &string to string*.
                if (e.offset == 0 && isSafePointerCast(e.var.type, pointee))
                {
                    // Create a CTFE pointer &var
                    auto ve = ctfeEmplaceExp!VarExp(e.loc, e.var);
                    ve.type = elemtype;
                    emplaceExp!(AddrExp)(pue, e.loc, ve, e.type);
                    result = pue.exp();
                    return;
                }
                error(e.loc, "reinterpreting cast from `%s` to `%s` is not supported in CTFE", val.type.toChars(), e.type.toChars());
                result = CTFEExp.cantexp;
                return;
            }

            const dinteger_t sz = pointee.size();
            dinteger_t indx = e.offset / sz;
            assert(sz * indx == e.offset);
            Expression aggregate = null;
            if (val.op == EXP.arrayLiteral || val.op == EXP.string_)
            {
                aggregate = val;
            }
            else if (auto se = val.isSliceExp())
            {
                aggregate = se.e1;
                UnionExp uelwr = void;
                Expression lwr = interpret(&uelwr, se.lwr, istate);
                indx += lwr.toInteger();
            }
            if (aggregate)
            {
                // Create a CTFE pointer &aggregate[ofs]
                auto ofs = ctfeEmplaceExp!IntegerExp(e.loc, indx, Type.tsize_t);
                auto ei = ctfeEmplaceExp!IndexExp(e.loc, aggregate, ofs);
                ei.type = elemtype;
                emplaceExp!(AddrExp)(pue, e.loc, ei, e.type);
                result = pue.exp();
                return;
            }
        }
        else if (e.offset == 0 && isSafePointerCast(e.var.type, pointee))
        {
            // Create a CTFE pointer &var
            auto ve = ctfeEmplaceExp!VarExp(e.loc, e.var);
            ve.type = e.var.type;
            emplaceExp!(AddrExp)(pue, e.loc, ve, e.type);
            result = pue.exp();
            return;
        }

        error(e.loc, "cannot convert `&%s` to `%s` at compile time", e.var.type.toChars(), e.type.toChars());
        result = CTFEExp.cantexp;
    }

    override void visit(AddrExp e)
    {
        debug (LOG)
        {
            printf("%s AddrExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (auto ve = e.e1.isVarExp())
        {
            Declaration decl = ve.var;

            // We cannot take the address of an imported symbol at compile time
            if (decl.isImportedSymbol())
            {
                error(e.loc, "cannot take address of imported symbol `%s` at compile time", decl.toChars());
                result = CTFEExp.cantexp;
                return;
            }

            if (decl.isDataseg())
            {
                // Normally this is already done by optimize()
                // Do it here in case optimize(WANTvalue) wasn't run before CTFE
                emplaceExp!(SymOffExp)(pue, e.loc, e.e1.isVarExp().var, 0);
                result = pue.exp();
                result.type = e.type;
                return;
            }
        }
        auto er = interpret(e.e1, istate, CTFEGoal.LValue);
        if (auto ve = er.isVarExp())
            if (istate && ve.var == istate.fd.vthis)
                er = interpret(er, istate);

        if (exceptionOrCant(er))
            return;

        // Return a simplified address expression
        emplaceExp!(AddrExp)(pue, e.loc, er, e.type);
        result = pue.exp();
    }

    override void visit(DelegateExp e)
    {
        debug (LOG)
        {
            printf("%s DelegateExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        // TODO: Really we should create a CTFE-only delegate expression
        // of a pointer and a funcptr.

        // If it is &nestedfunc, just return it
        // TODO: We should save the context pointer
        if (auto ve1 = e.e1.isVarExp())
            if (ve1.var == e.func)
            {
                result = e;
                return;
            }

        auto er = interpret(pue, e.e1, istate);
        if (exceptionOrCant(er))
            return;
        if (er == e.e1)
        {
            // If it has already been CTFE'd, just return it
            result = e;
        }
        else
        {
            er = (er == pue.exp()) ? pue.copy() : er;
            emplaceExp!(DelegateExp)(pue, e.loc, er, e.func, false);
            result = pue.exp();
            result.type = e.type;
        }
    }

    static Expression getVarExp(Loc loc, InterState* istate, Declaration d, CTFEGoal goal)
    {
        Expression e = CTFEExp.cantexp;
        if (VarDeclaration v = d.isVarDeclaration())
        {
            /* Magic variable __ctfe always returns true when interpreting
             */
            if (v.ident == Id.ctfe)
                return IntegerExp.createBool(true);

            if (!v.originalType && v.semanticRun < PASS.semanticdone) // semantic() not yet run
            {
                v.dsymbolSemantic(null);
                if (v.type.ty == Terror)
                    return CTFEExp.cantexp;
            }

            if ((v.isConst() || v.isImmutable() || v.storage_class & STC.manifest) && !hasValue(v) && v._init && !v.isCTFE())
            {
                if (v.inuse)
                {
                    error(loc, "circular initialization of %s `%s`", v.kind(), v.toPrettyChars());
                    return CTFEExp.cantexp;
                }
                if (v._scope)
                {
                    v.inuse++;
                    v._init = v._init.initializerSemantic(v._scope, v.type, INITinterpret); // might not be run on aggregate members
                    v.inuse--;
                }
                e = v._init.initializerToExpression(v.type);
                if (!e)
                    return CTFEExp.cantexp;
                assert(e.type);

                // There's a terrible hack in `dmd.dsymbolsem` that special case
                // a struct with all zeros to an `ExpInitializer(BlitExp(IntegerExp(0)))`
                // There's matching code for it in e2ir (toElem's visitAssignExp),
                // so we need the same hack here.
                // This does not trigger for global as they get a normal initializer.
                if (auto ts = e.type.isTypeStruct())
                    if (auto ae = e.isBlitExp())
                        if (ae.e2.op == EXP.int64)
                            e = ts.defaultInitLiteral(loc);

                if (e.op == EXP.construct || e.op == EXP.blit)
                {
                    AssignExp ae = cast(AssignExp)e;
                    e = ae.e2;
                }

                if (e.op == EXP.error)
                {
                    // FIXME: Ultimately all errors should be detected in prior semantic analysis stage.
                }
                else if (v.isDataseg() || (v.storage_class & STC.manifest))
                {
                    /* https://issues.dlang.org/show_bug.cgi?id=14304
                     * e is a value that is not yet owned by CTFE.
                     * Mark as "cached", and use it directly during interpretation.
                     */
                    e = scrubCacheValue(e);
                    ctfeGlobals.stack.saveGlobalConstant(v, e);
                }
                else
                {
                    v.inuse++;
                    e = interpret(e, istate);
                    v.inuse--;
                    if (CTFEExp.isCantExp(e) && !global.gag && !ctfeGlobals.stackTraceCallsToSuppress)
                        errorSupplemental(loc, "while evaluating %s.init", v.toChars());
                    if (exceptionOrCantInterpret(e))
                        return e;
                }
            }
            else if (v.isCTFE() && !hasValue(v))
            {
                if (v._init && v.type.size() != 0)
                {
                    if (v._init.isVoidInitializer())
                    {
                        // var should have been initialized when it was created
                        error(loc, "CTFE internal error: trying to access uninitialized var");
                        assert(0);
                    }
                    e = v._init.initializerToExpression();
                }
                else
                    // Zero-length arrays don't have an initializer
                    e = v.type.defaultInitLiteral(e.loc);

                e = interpret(e, istate);
            }
            else if (!(v.isDataseg() || v.storage_class & STC.manifest) && !v.isCTFE() && !istate)
            {
                error(loc, "variable `%s` cannot be read at compile time", v.toChars());
                return CTFEExp.cantexp;
            }
            else
            {
                e = hasValue(v) ? getValue(v) : null;
                if (!e)
                {
                    // Zero-length arrays don't have an initializer
                    if (v.type.size() == 0)
                        e = v.type.defaultInitLiteral(loc);
                    else if (!v.isCTFE() && v.isDataseg())
                    {
                        error(loc, "static variable `%s` cannot be read at compile time", v.toChars());
                        return CTFEExp.cantexp;
                    }
                    else
                    {
                        assert(!(v._init && v._init.isVoidInitializer()));
                        // CTFE initiated from inside a function
                        error(loc, "variable `%s` cannot be read at compile time", v.toChars());
                        return CTFEExp.cantexp;
                    }
                }
                if (auto vie = e.isVoidInitExp())
                {
                    error(loc, "cannot read uninitialized variable `%s` in ctfe", v.toPrettyChars());
                    errorSupplemental(vie.var.loc, "`%s` was uninitialized and used before set", vie.var.toChars());
                    return CTFEExp.cantexp;
                }
                if (goal != CTFEGoal.LValue && v.isReference())
                    e = interpret(e, istate, goal);
            }
            if (!e)
                e = CTFEExp.cantexp;
        }
        else if (SymbolDeclaration s = d.isSymbolDeclaration())
        {
            // exclude void[]-typed `__traits(initSymbol)`
            if (auto ta = s.type.toBasetype().isTypeDArray())
            {
                assert(ta.next.ty == Tvoid);
                error(loc, "cannot determine the address of the initializer symbol during CTFE");
                return CTFEExp.cantexp;
            }

            // Struct static initializers, for example
            e = s.dsym.type.defaultInitLiteral(loc);
            if (e.op == EXP.error)
                error(loc, "CTFE failed because of previous errors in `%s.init`", s.toChars());
            e = e.expressionSemantic(null);
            if (e.op == EXP.error)
                e = CTFEExp.cantexp;
            else // Convert NULL to CTFEExp
                e = interpret(e, istate, goal);
        }
        else
            error(loc, "cannot interpret declaration `%s` at compile time", d.toChars());
        return e;
    }

    override void visit(VarExp e)
    {
        debug (LOG)
        {
            printf("%s VarExp::interpret() `%s`, goal = %d\n", e.loc.toChars(), e.toChars(), goal);
        }
        if (e.var.isFuncDeclaration())
        {
            result = e;
            return;
        }

        if (goal == CTFEGoal.LValue)
        {
            if (auto v = e.var.isVarDeclaration())
            {
                if (!hasValue(v))
                {
                    // Compile-time known non-CTFE variable from an outer context
                    // e.g. global or from a ref argument
                    if (v.isConst() || v.isImmutable())
                    {
                        result = getVarExp(e.loc, istate, v, goal);
                        return;
                    }

                    if (!v.isCTFE() && v.isDataseg())
                        error(e.loc, "static variable `%s` cannot be read at compile time", v.toChars());
                    else // CTFE initiated from inside a function
                        error(e.loc, "variable `%s` cannot be read at compile time", v.toChars());
                    result = CTFEExp.cantexp;
                    return;
                }

                if (v.storage_class & (STC.out_ | STC.ref_))
                {
                    // Strip off the nest of ref variables
                    Expression ev = getValue(v);
                    if (ev.op == EXP.variable ||
                        ev.op == EXP.index ||
                        (ev.op == EXP.slice && ev.type.toBasetype().ty == Tsarray) ||
                        ev.op == EXP.dotVariable)
                    {
                        result = interpret(pue, ev, istate, goal);
                        return;
                    }
                }
            }
            result = e;
            return;
        }
        result = getVarExp(e.loc, istate, e.var, goal);
        if (exceptionOrCant(result))
            return;

        // Visit the default initializer for noreturn variables
        // (Custom initializers would abort the current function call and exit above)
        if (result.type.ty == Tnoreturn)
        {
            result.accept(this);
            return;
        }

        if ((e.var.storage_class & (STC.ref_ | STC.out_)) == 0 && e.type.baseElemOf().ty != Tstruct)
        {
            /* Ultimately, STC.ref_|STC.out_ check should be enough to see the
             * necessity of type repainting. But currently front-end paints
             * non-ref struct variables by the const type.
             *
             *  auto foo(ref const S cs);
             *  S s;
             *  foo(s); // VarExp('s') will have const(S)
             */
            // A VarExp may include an implicit cast. It must be done explicitly.
            result = paintTypeOntoLiteral(pue, e.type, result);
        }
    }

    override void visit(DeclarationExp e)
    {
        debug (LOG)
        {
            printf("%s DeclarationExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Dsymbol s = e.declaration;
        while (s.isAttribDeclaration())
        {
            auto ad = cast(AttribDeclaration)s;
            assert(ad.decl && ad.decl.length == 1); // Currently, only one allowed when parsing
            s = (*ad.decl)[0];
        }
        if (VarDeclaration v = s.isVarDeclaration())
        {
            if (TupleDeclaration td = v.toAlias().isTupleDeclaration())
            {
                result = null;

                // Reserve stack space for all tuple members
                td.foreachVar((s)
                {
                    VarDeclaration v2 = s.isVarDeclaration();
                    assert(v2);
                    if (v2.isDataseg() && !v2.isCTFE())
                        return 0;

                    ctfeGlobals.stack.push(v2);
                    if (v2._init)
                    {
                        Expression einit;
                        if (ExpInitializer ie = v2._init.isExpInitializer())
                        {
                            einit = interpretRegion(ie.exp, istate, goal);
                            if (exceptionOrCant(einit))
                                return 1;
                        }
                        else if (v2._init.isVoidInitializer())
                        {
                            einit = voidInitLiteral(v2.type, v2).copy();
                        }
                        else
                        {
                            error(e.loc, "declaration `%s` is not yet implemented in CTFE", e.toChars());
                            result = CTFEExp.cantexp;
                            return 1;
                        }
                        setValue(v2, einit);
                    }
                    return 0;
                });
                return;
            }
            if (v.isStatic())
            {
                // Just ignore static variables which aren't read or written yet
                result = null;
                return;
            }
            if (!(v.isDataseg() || v.storage_class & STC.manifest) || v.isCTFE())
                ctfeGlobals.stack.push(v);
            if (v._init)
            {
                if (ExpInitializer ie = v._init.isExpInitializer())
                {
                    result = interpretRegion(ie.exp, istate, goal);
                    return;
                }
                else if (v._init.isVoidInitializer())
                {
                    result = voidInitLiteral(v.type, v).copy();
                    // There is no AssignExp for void initializers,
                    // so set it here.
                    setValue(v, result);
                    return;
                }
                else if (v._init.isArrayInitializer())
                {
                    result = v._init.initializerToExpression(v.type);
                    if (result !is null)
                        return;
                }
                error(e.loc, "declaration `%s` is not yet implemented in CTFE", e.toChars());
                result = CTFEExp.cantexp;
            }
            else if (v.type.size() == 0)
            {
                // Zero-length arrays don't need an initializer
                result = v.type.defaultInitLiteral(e.loc);
            }
            else
            {
                error(e.loc, "variable `%s` cannot be modified at compile time", v.toChars());
                result = CTFEExp.cantexp;
            }
            return;
        }
        if (s.isTemplateMixin() || s.isTupleDeclaration())
        {
            // These can be made to work, too lazy now
            error(e.loc, "declaration `%s` is not yet implemented in CTFE", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        // Others should not contain executable code, so are trivial to evaluate
        result = null;
        debug (LOG)
        {
            printf("-DeclarationExp::interpret(%s): %p\n", e.toChars(), result);
        }
    }

    override void visit(TypeidExp e)
    {
        debug (LOG)
        {
            printf("%s TypeidExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (Type t = isType(e.obj))
        {
            result = e;
            return;
        }
        if (Expression ex = isExpression(e.obj))
        {
            result = interpret(pue, ex, istate);
            if (exceptionOrCant(ex))
                return;

            if (result.op == EXP.null_)
            {
                error(e.loc, "null pointer dereference evaluating typeid. `%s` is `null`", ex.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            if (result.op != EXP.classReference)
            {
                error(e.loc, "CTFE internal error: determining classinfo");
                result = CTFEExp.cantexp;
                return;
            }

            ClassDeclaration cd = result.isClassReferenceExp().originalClass();
            assert(cd);

            emplaceExp!(TypeidExp)(pue, e.loc, cd.type);
            result = pue.exp();
            result.type = e.type;
            return;
        }
        visit(cast(Expression)e);
    }

    override void visit(TupleExp e)
    {
        debug (LOG)
        {
            printf("%s TupleExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (exceptionOrCant(interpretRegion(e.e0, istate, CTFEGoal.Nothing)))
            return;

        auto expsx = e.exps;
        foreach (i, exp; *expsx)
        {
            Expression ex = interpretRegion(exp, istate);
            if (exceptionOrCant(ex))
                return;

            // A tuple of assignments can contain void (Bug 5676).
            if (goal == CTFEGoal.Nothing)
                continue;
            if (ex.op == EXP.voidExpression)
            {
                error(e.loc, "CTFE internal error: void element `%s` in sequence", exp.toChars());
                assert(0);
            }

            /* If any changes, do Copy On Write
             */
            if (ex !is exp)
            {
                expsx = copyArrayOnWrite(expsx, e.exps);
                (*expsx)[i] = copyRegionExp(ex);
            }
        }

        if (expsx !is e.exps)
        {
            expandTuples(expsx);
            emplaceExp!(TupleExp)(pue, e.loc, expsx);
            result = pue.exp();
            result.type = new TypeTuple(expsx);
        }
        else
            result = e;
    }

    override void visit(ArrayLiteralExp e)
    {
        debug (LOG)
        {
            printf("%s ArrayLiteralExp::interpret() %s, %s\n", e.loc.toChars(), e.type.toChars(), e.toChars());
        }
        if (e.ownedByCtfe >= OwnedBy.ctfe) // We've already interpreted all the elements
        {
            result = e;
            return;
        }

        Type tb = e.type.toBasetype();
        Type tn = tb.nextOf().toBasetype();
        bool wantCopy = (tn.ty == Tsarray || tn.ty == Tstruct);

        auto basis = interpretRegion(e.basis, istate);
        if (exceptionOrCant(basis))
            return;

        auto expsx = e.elements;
        size_t dim = expsx ? expsx.length : 0;

        for (size_t i = 0; i < dim; i++)
        {
            Expression exp = (*expsx)[i];
            Expression ex;
            if (!exp)
            {
                ex = copyLiteral(basis).copy();
            }
            else
            {
                // segfault bug 6250
                assert(exp.op != EXP.index || exp.isIndexExp().e1 != e);

                ex = interpretRegion(exp, istate);
                if (exceptionOrCant(ex))
                    return;

                /* Each elements should have distinct CTFE memory.
                 *  int[1] z = 7;
                 *  int[1][] pieces = [z,z];    // here
                 */
                if (wantCopy)
                    ex = copyLiteral(ex).copy();
            }

            /* If any changes, do Copy On Write
             */
            if (ex !is exp)
            {
                expsx = copyArrayOnWrite(expsx, e.elements);
                (*expsx)[i] = ex;
            }
        }

        if (expsx !is e.elements)
        {
            // todo: all tuple expansions should go in semantic phase.
            expandTuples(expsx);
            if (expsx.length != dim)
            {
                error(e.loc, "CTFE internal error: invalid array literal");
                result = CTFEExp.cantexp;
                return;
            }
            emplaceExp!(ArrayLiteralExp)(pue, e.loc, e.type, basis, expsx);
            auto ale = pue.exp().isArrayLiteralExp();
            ale.ownedByCtfe = OwnedBy.ctfe;
            result = ale;
        }
        else if ((cast(TypeNext)e.type).next.mod & (MODFlags.const_ | MODFlags.immutable_))
        {
            // If it's immutable, we don't need to dup it
            result = e;
        }
        else
        {
            *pue = copyLiteral(e);
            result = pue.exp();
        }
    }

    override void visit(AssocArrayLiteralExp e)
    {
        debug (LOG)
        {
            printf("%s AssocArrayLiteralExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.ownedByCtfe >= OwnedBy.ctfe) // We've already interpreted all the elements
        {
            result = e;
            return;
        }

        auto keysx = e.keys;
        auto valuesx = e.values;
        foreach (i, ekey; *keysx)
        {
            auto evalue = (*valuesx)[i];

            auto ek = interpretRegion(ekey, istate);
            if (exceptionOrCant(ek))
                return;
            auto ev = interpretRegion(evalue, istate);
            if (exceptionOrCant(ev))
                return;

            /* If any changes, do Copy On Write
             */
            if (ek !is ekey ||
                ev !is evalue)
            {
                keysx = copyArrayOnWrite(keysx, e.keys);
                valuesx = copyArrayOnWrite(valuesx, e.values);
                (*keysx)[i] = ek;
                (*valuesx)[i] = ev;
            }
        }
        if (keysx !is e.keys)
            expandTuples(keysx);
        if (valuesx !is e.values)
            expandTuples(valuesx);
        if (keysx.length != valuesx.length)
        {
            error(e.loc, "CTFE internal error: invalid AA");
            result = CTFEExp.cantexp;
            return;
        }

        /* Remove duplicate keys
         */
        for (size_t i = 1; i < keysx.length; i++)
        {
            auto ekey = (*keysx)[i - 1];
            for (size_t j = i; j < keysx.length; j++)
            {
                auto ekey2 = (*keysx)[j];
                if (!ctfeEqual(e.loc, EXP.equal, ekey, ekey2))
                    continue;

                // Remove ekey
                keysx = copyArrayOnWrite(keysx, e.keys);
                valuesx = copyArrayOnWrite(valuesx, e.values);
                keysx.remove(i - 1);
                valuesx.remove(i - 1);

                i -= 1; // redo the i'th iteration
                break;
            }
        }

        if (keysx !is e.keys ||
            valuesx !is e.values)
        {
            assert(keysx !is e.keys &&
                   valuesx !is e.values);
            auto aae = ctfeEmplaceExp!AssocArrayLiteralExp(e.loc, keysx, valuesx);
            aae.type = e.type;
            aae.ownedByCtfe = OwnedBy.ctfe;
            result = aae;
        }
        else
        {
            *pue = copyLiteral(e);
            result = pue.exp();
        }
    }

    override void visit(StructLiteralExp e)
    {
        debug (LOG)
        {
            printf("%s StructLiteralExp::interpret() %s ownedByCtfe = %d\n", e.loc.toChars(), e.toChars(), e.ownedByCtfe);
        }
        if (e.ownedByCtfe >= OwnedBy.ctfe)
        {
            result = e;
            return;
        }

        size_t dim = e.elements ? e.elements.length : 0;
        auto expsx = e.elements;

        if (dim != e.sd.fields.length)
        {
            // guaranteed by AggregateDeclaration.fill and TypeStruct.defaultInitLiteral
            const nvthis = e.sd.fields.length - e.sd.nonHiddenFields();
            assert(e.sd.fields.length - dim == nvthis);

            /* If a nested struct has no initialized hidden pointer,
             * set it to null to match the runtime behaviour.
             */
            foreach (const i; 0 .. nvthis)
            {
                auto ne = ctfeEmplaceExp!NullExp(e.loc);
                auto vthis = i == 0 ? e.sd.vthis : e.sd.vthis2;
                ne.type = vthis.type;

                expsx = copyArrayOnWrite(expsx, e.elements);
                expsx.push(ne);
                ++dim;
            }
        }
        assert(dim == e.sd.fields.length);

        foreach (i; 0 .. dim)
        {
            auto v = e.sd.fields[i];
            Expression exp = (*expsx)[i];
            Expression ex;
            if (!exp)
            {
                ex = voidInitLiteral(v.type, v).copy();
            }
            else
            {
                ex = interpretRegion(exp, istate);
                if (exceptionOrCant(ex))
                    return;
                if ((v.type.ty != ex.type.ty) && v.type.ty == Tsarray)
                {
                    // Block assignment from inside struct literals
                    auto tsa = cast(TypeSArray)v.type;
                    auto len = cast(size_t)tsa.dim.toInteger();
                    UnionExp ue = void;
                    ex = createBlockDuplicatedArrayLiteral(&ue, ex.loc, v.type, ex, len);
                    if (ex == ue.exp())
                        ex = ue.copy();
                }
            }

            /* If any changes, do Copy On Write
             */
            if (ex !is exp)
            {
                expsx = copyArrayOnWrite(expsx, e.elements);
                (*expsx)[i] = ex;
            }
        }

        if (expsx !is e.elements)
        {
            expandTuples(expsx);
            if (expsx.length != e.sd.fields.length)
            {
                error(e.loc, "CTFE internal error: invalid struct literal");
                result = CTFEExp.cantexp;
                return;
            }
            emplaceExp!(StructLiteralExp)(pue, e.loc, e.sd, expsx);
            auto sle = pue.exp().isStructLiteralExp();
            sle.type = e.type;
            sle.ownedByCtfe = OwnedBy.ctfe;
            sle.origin = e.origin;
            result = sle;
        }
        else
        {
            *pue = copyLiteral(e);
            result = pue.exp();
        }
    }

    // Create an array literal of type 'newtype' with dimensions given by
    // 'arguments'[argnum..$]
    static Expression recursivelyCreateArrayLiteral(UnionExp* pue, Loc loc, Type newtype, InterState* istate, Expressions* arguments, int argnum)
    {
        Expression lenExpr = interpret(pue, (*arguments)[argnum], istate);
        if (exceptionOrCantInterpret(lenExpr))
            return lenExpr;
        size_t len = cast(size_t)lenExpr.toInteger();
        Type elemType = (cast(TypeArray)newtype).next;
        if (elemType.ty == Tarray && argnum < arguments.length - 1)
        {
            Expression elem = recursivelyCreateArrayLiteral(pue, loc, elemType, istate, arguments, argnum + 1);
            if (exceptionOrCantInterpret(elem))
                return elem;

            auto elements = new Expressions(len);
            foreach (ref element; *elements)
                element = copyLiteral(elem).copy();
            emplaceExp!(ArrayLiteralExp)(pue, loc, newtype, elements);
            auto ae = pue.exp().isArrayLiteralExp();
            ae.ownedByCtfe = OwnedBy.ctfe;
            return ae;
        }
        assert(argnum == arguments.length - 1);
        if (elemType.ty.isSomeChar)
        {
            const ch = cast(dchar)elemType.defaultInitLiteral(loc).toInteger();
            const sz = cast(ubyte)elemType.size();
            return createBlockDuplicatedStringLiteral(pue, loc, newtype, ch, len, sz);
        }
        else
        {
            auto el = interpret(elemType.defaultInitLiteral(loc), istate);
            return createBlockDuplicatedArrayLiteral(pue, loc, newtype, el, len);
        }
    }

    override void visit(NewExp e)
    {
        debug (LOG)
        {
            printf("%s NewExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }

        if (e.placement)
        {
            error(e.placement.loc, "`new ( %s )` PlacementExpression cannot be evaluated at compile time", e.placement.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        Expression epre = interpret(pue, e.argprefix, istate, CTFEGoal.Nothing);
        if (exceptionOrCant(epre))
            return;

        if (e.newtype.ty == Tarray && e.arguments)
        {
            result = recursivelyCreateArrayLiteral(pue, e.loc, e.newtype, istate, e.arguments, 0);
            return;
        }
        if (auto ts = e.newtype.toBasetype().isTypeStruct())
        {
            if (e.member)
            {
                Expression se = e.newtype.defaultInitLiteral(e.loc);
                se = interpret(se, istate);
                if (exceptionOrCant(se))
                    return;
                result = interpretFunction(pue, e.member, istate, e.arguments, se);

                // Repaint as same as CallExp::interpret() does.
                result.loc = e.loc;
            }
            else
            {
                StructDeclaration sd = ts.sym;
                auto exps = new Expressions();
                exps.reserve(sd.fields.length);
                if (e.arguments)
                {
                    exps.setDim(e.arguments.length);
                    foreach (i, ex; *e.arguments)
                    {
                        ex = interpretRegion(ex, istate);
                        if (exceptionOrCant(ex))
                            return;
                        (*exps)[i] = ex;
                    }
                }
                sd.fill(e.loc, *exps, false);

                auto se = ctfeEmplaceExp!StructLiteralExp(e.loc, sd, exps, e.newtype);
                se.origin = se;
                se.type = e.newtype;
                se.ownedByCtfe = OwnedBy.ctfe;
                result = interpret(pue, se, istate);
            }
            if (exceptionOrCant(result))
                return;
            Expression ev = (result == pue.exp()) ? pue.copy() : result;
            emplaceExp!(AddrExp)(pue, e.loc, ev, e.type);
            result = pue.exp();
            return;
        }
        if (auto tc = e.newtype.toBasetype().isTypeClass())
        {
            ClassDeclaration cd = tc.sym;
            size_t totalFieldCount = 0;
            for (ClassDeclaration c = cd; c; c = c.baseClass)
                totalFieldCount += c.fields.length;
            auto elems = new Expressions(totalFieldCount);
            size_t fieldsSoFar = totalFieldCount;
            for (ClassDeclaration c = cd; c; c = c.baseClass)
            {
                fieldsSoFar -= c.fields.length;
                foreach (i, v; c.fields)
                {
                    if (v.inuse)
                    {
                        error(e.loc, "circular reference to `%s`", v.toPrettyChars());
                        result = CTFEExp.cantexp;
                        return;
                    }
                    Expression m;
                    if (v._init)
                    {
                        if (v._init.isVoidInitializer())
                            m = voidInitLiteral(v.type, v).copy();
                        else
                            m = v.getConstInitializer(true);
                    }
                    else if (v.type.isTypeNoreturn())
                    {
                        // Noreturn field with default initializer
                        (*elems)[fieldsSoFar + i] = null;
                        continue;
                    }
                    else
                        m = v.type.defaultInitLiteral(e.loc);
                    if (exceptionOrCant(m))
                        return;
                    (*elems)[fieldsSoFar + i] = copyLiteral(m).copy();
                }
            }
            // Hack: we store a ClassDeclaration instead of a StructDeclaration.
            // We probably won't get away with this.
//            auto se = new StructLiteralExp(e.loc, cast(StructDeclaration)cd, elems, e.newtype);
            auto se = ctfeEmplaceExp!StructLiteralExp(e.loc, cast(StructDeclaration)cd, elems, e.newtype);
            se.origin = se;
            se.ownedByCtfe = OwnedBy.ctfe;
            Expression eref = ctfeEmplaceExp!ClassReferenceExp(e.loc, se, e.type);
            if (e.member)
            {
                // Call constructor
                if (!e.member.fbody)
                {
                    Expression ctorfail = evaluateIfBuiltin(pue, istate, e.loc, e.member, e.arguments, eref);
                    if (ctorfail)
                    {
                        if (exceptionOrCant(ctorfail))
                            return;
                        result = eref;
                        return;
                    }
                    auto m = e.member;
                    error(m.loc, "%s `%s` `%s` cannot be constructed at compile time, because the constructor has no available source code",
                        m.kind, m.toPrettyChars, e.newtype.toChars());
                    result = CTFEExp.cantexp;
                    return;
                }
                UnionExp ue = void;
                Expression ctorfail = interpretFunction(&ue, e.member, istate, e.arguments, eref);
                if (exceptionOrCant(ctorfail))
                    return;

                /* https://issues.dlang.org/show_bug.cgi?id=14465
                 * Repaint the loc, because a super() call
                 * in the constructor modifies the loc of ClassReferenceExp
                 * in CallExp::interpret().
                 */
                eref.loc = e.loc;
            }
            result = eref;
            return;
        }
        if (e.newtype.toBasetype().isScalar())
        {
            Expression newval;
            if (e.arguments && e.arguments.length)
                newval = (*e.arguments)[0];
            else
                newval = e.newtype.defaultInitLiteral(e.loc);
            newval = interpretRegion(newval, istate);
            if (exceptionOrCant(newval))
                return;

            // Create a CTFE pointer &[newval][0]
            auto elements = new Expressions(1);
            (*elements)[0] = newval;
            auto ae = ctfeEmplaceExp!ArrayLiteralExp(e.loc, e.newtype.arrayOf(), elements);
            ae.ownedByCtfe = OwnedBy.ctfe;

            auto ei = ctfeEmplaceExp!IndexExp(e.loc, ae, ctfeEmplaceExp!IntegerExp(Loc.initial, 0, Type.tsize_t));
            ei.type = e.newtype;
            emplaceExp!(AddrExp)(pue, e.loc, ei, e.type);
            result = pue.exp();
            return;
        }
        error(e.loc, "cannot interpret `%s` at compile time", e.toChars());
        result = CTFEExp.cantexp;
    }

    override void visit(UnaExp e)
    {
        debug (LOG)
        {
            printf("%s UnaExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        UnionExp ue = void;
        Expression e1 = interpret(&ue, e.e1, istate);
        if (exceptionOrCant(e1))
            return;
        switch (e.op)
        {
        case EXP.negate:
            *pue = Neg(e.type, e1);
            break;

        case EXP.tilde:
            *pue = Com(e.type, e1);
            break;

        case EXP.not:
            *pue = Not(e.type, e1);
            break;

        default:
            assert(0);
        }
        result = (*pue).exp();
    }

    override void visit(DotTypeExp e)
    {
        debug (LOG)
        {
            printf("%s DotTypeExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        UnionExp ue = void;
        Expression e1 = interpret(&ue, e.e1, istate);
        if (exceptionOrCant(e1))
            return;
        if (e1 == e.e1)
            result = e; // optimize: reuse this CTFE reference
        else
        {
            auto edt = e.copy().isDotTypeExp();
            edt.e1 = (e1 == ue.exp()) ? e1.copy() : e1; // don't return pointer to ue
            result = edt;
        }
    }

    private alias fp_t = extern (D) UnionExp function(Loc loc, Type, Expression, Expression);
    private alias fp2_t = extern (D) bool function(Loc loc, EXP, Expression, Expression);

    extern (D) private void interpretCommon(BinExp e, fp_t fp)
    {
        debug (LOG)
        {
            printf("%s BinExp::interpretCommon() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.e1.type.ty == Tpointer && e.e2.type.ty == Tpointer && e.op == EXP.min)
        {
            UnionExp ue1 = void;
            Expression e1 = interpret(&ue1, e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            UnionExp ue2 = void;
            Expression e2 = interpret(&ue2, e.e2, istate);
            if (exceptionOrCant(e2))
                return;
            result = pointerDifference(pue, e.loc, e.type, e1, e2);
            return;
        }
        if (e.e1.type.ty == Tpointer && e.e2.type.isIntegral())
        {
            UnionExp ue1 = void;
            Expression e1 = interpret(&ue1, e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            UnionExp ue2 = void;
            Expression e2 = interpret(&ue2, e.e2, istate);
            if (exceptionOrCant(e2))
                return;
            result = pointerArithmetic(pue, e.loc, e.op, e.type, e1, e2);
            return;
        }
        if (e.e2.type.ty == Tpointer && e.e1.type.isIntegral() && e.op == EXP.add)
        {
            UnionExp ue1 = void;
            Expression e1 = interpret(&ue1, e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            UnionExp ue2 = void;
            Expression e2 = interpret(&ue2, e.e2, istate);
            if (exceptionOrCant(e2))
                return;
            result = pointerArithmetic(pue, e.loc, e.op, e.type, e2, e1);
            return;
        }
        if (e.e1.type.ty == Tpointer || e.e2.type.ty == Tpointer)
        {
            error(e.loc, "pointer expression `%s` cannot be interpreted at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        bool evalOperand(UnionExp* pue, Expression ex, out Expression er)
        {
            er = interpret(pue, ex, istate);
            if (exceptionOrCant(er))
                return false;
            return true;
        }

        UnionExp ue1 = void;
        Expression e1;
        if (!evalOperand(&ue1, e.e1, e1))
            return;

        UnionExp ue2 = void;
        Expression e2;
        if (!evalOperand(&ue2, e.e2, e2))
            return;

        if (e.op == EXP.rightShift || e.op == EXP.leftShift || e.op == EXP.unsignedRightShift)
        {
            const sinteger_t i2 = e2.toInteger();
            const uinteger_t sz = e1.type.size() * 8;
            if (i2 < 0 || i2 >= sz)
            {
                error(e.loc, "shift by %lld is outside the range 0..%llu", i2, cast(ulong)sz - 1);
                result = CTFEExp.cantexp;
                return;
            }
        }

        /******************************************
         * Perform the operation fp on operands e1 and e2.
         */
        UnionExp evaluate(Loc loc, Type type, Expression e1, Expression e2)
        {
            UnionExp ue = void;
            auto ae1 = e1.isArrayLiteralExp();
            auto ae2 = e2.isArrayLiteralExp();
            if (ae1 || ae2)
            {
                /* Cases:
                 * 1. T[] op T[]
                 * 2. T op T[]
                 * 3. T[] op T
                 */
                if (ae1 && e2.implicitConvTo(e1.type.toBasetype().nextOf())) // case 3
                    ae2 = null;
                else if (ae2 && e1.implicitConvTo(e2.type.toBasetype().nextOf())) // case 2
                    ae1 = null;
                // else case 1

                auto aex = ae1 ? ae1 : ae2;
                if (!aex.elements)
                {
                    emplaceExp!ArrayLiteralExp(&ue, loc, type, cast(Expressions*) null);
                    return ue;
                }
                const length = aex.elements.length;
                Expressions* elements = new Expressions(length);

                emplaceExp!ArrayLiteralExp(&ue, loc, type, elements);
                foreach (i; 0 .. length)
                {
                    Expression e1x = ae1 ? ae1[i] : e1;
                    Expression e2x = ae2 ? ae2[i] : e2;
                    UnionExp uex = evaluate(loc, e1x.type, e1x, e2x);
                    // This can be made more efficient by making use of ue.basis
                    (*elements)[i] = uex.copy();
                }
                return ue;
            }

            if (e1.isConst() != 1)
            {
                // The following should really be an assert()
                error(e1.loc, "CTFE internal error: non-constant value `%s`", e1.toChars());
                emplaceExp!CTFEExp(&ue, EXP.cantExpression);
                return ue;
            }
            if (e2.isConst() != 1)
            {
                error(e2.loc, "CTFE internal error: non-constant value `%s`", e2.toChars());
                emplaceExp!CTFEExp(&ue, EXP.cantExpression);
                return ue;
            }

            return (*fp)(loc, type, e1, e2);
        }

        *pue = evaluate(e.loc, e.type, e1, e2);
        result = (*pue).exp();
        if (CTFEExp.isCantExp(result))
            error(e.loc, "`%s` cannot be interpreted at compile time", e.toChars());
    }

    extern (D) private void interpretCompareCommon(BinExp e, fp2_t fp)
    {
        debug (LOG)
        {
            printf("%s BinExp::interpretCompareCommon() %s\n", e.loc.toChars(), e.toChars());
        }
        UnionExp ue1 = void;
        UnionExp ue2 = void;
        if (e.e1.type.ty == Tpointer && e.e2.type.ty == Tpointer)
        {
            Expression e1 = interpret(&ue1, e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            Expression e2 = interpret(&ue2, e.e2, istate);
            if (exceptionOrCant(e2))
                return;
            //printf("e1 = %s %s, e2 = %s %s\n", e1.type.toChars(), e1.toChars(), e2.type.toChars(), e2.toChars());
            dinteger_t ofs1, ofs2;
            Expression agg1 = getAggregateFromPointer(e1, &ofs1);
            Expression agg2 = getAggregateFromPointer(e2, &ofs2);
            //printf("agg1 = %p %s, agg2 = %p %s\n", agg1, agg1.toChars(), agg2, agg2.toChars());
            const cmp = comparePointers(e.op, agg1, ofs1, agg2, ofs2);
            if (cmp == -1)
            {
                char dir = (e.op == EXP.greaterThan || e.op == EXP.greaterOrEqual) ? '<' : '>';
                error(e.loc, "the ordering of pointers to unrelated memory blocks is indeterminate in CTFE. To check if they point to the same memory block, use both `>` and `<` inside `&&` or `||`, eg `%s && %s %c= %s + 1`", e.toChars(), e.e1.toChars(), dir, e.e2.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            if (e.type.equals(Type.tbool))
                result = IntegerExp.createBool(cmp != 0);
            else
            {
                emplaceExp!(IntegerExp)(pue, e.loc, cmp, e.type);
                result = (*pue).exp();
            }
            return;
        }
        Expression e1 = interpret(&ue1, e.e1, istate);
        if (exceptionOrCant(e1))
            return;
        if (!isCtfeComparable(e1))
        {
            error(e.loc, "cannot compare `%s` at compile time", e1.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        Expression e2 = interpret(&ue2, e.e2, istate);
        if (exceptionOrCant(e2))
            return;
        if (!isCtfeComparable(e2))
        {
            error(e.loc, "cannot compare `%s` at compile time", e2.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        const cmp = (*fp)(e.loc, e.op, e1, e2);
        if (e.type.equals(Type.tbool))
            result = IntegerExp.createBool(cmp);
        else
        {
            emplaceExp!(IntegerExp)(pue, e.loc, cmp, e.type);
            result = (*pue).exp();
        }
    }

    override void visit(BinExp e)
    {
        switch (e.op)
        {
        case EXP.add:
            interpretCommon(e, &Add);
            return;

        case EXP.min:
            interpretCommon(e, &Min);
            return;

        case EXP.mul:
            interpretCommon(e, &Mul);
            return;

        case EXP.div:
            interpretCommon(e, &Div);
            return;

        case EXP.mod:
            interpretCommon(e, &Mod);
            return;

        case EXP.leftShift:
            interpretCommon(e, &Shl);
            return;

        case EXP.rightShift:
            interpretCommon(e, &Shr);
            return;

        case EXP.unsignedRightShift:
            interpretCommon(e, &Ushr);
            return;

        case EXP.and:
            interpretCommon(e, &And);
            return;

        case EXP.or:
            interpretCommon(e, &Or);
            return;

        case EXP.xor:
            interpretCommon(e, &Xor);
            return;

        case EXP.pow:
            interpretCommon(e, &Pow);
            return;

        case EXP.equal:
        case EXP.notEqual:
            interpretCompareCommon(e, &ctfeEqual);
            return;

        case EXP.identity:
        case EXP.notIdentity:
            interpretCompareCommon(e, &ctfeIdentity);
            return;

        case EXP.lessThan:
        case EXP.lessOrEqual:
        case EXP.greaterThan:
        case EXP.greaterOrEqual:
            interpretCompareCommon(e, &ctfeCmp);
            return;

        default:
            printf("be = '%s' %s at [%s]\n", EXPtoString(e.op).ptr, e.toChars(), e.loc.toChars());
            assert(0);
        }
    }

    /* Helper functions for BinExp::interpretAssignCommon
     */
    // Returns the variable which is eventually modified, or NULL if an rvalue.
    // thisval is the current value of 'this'.
    static VarDeclaration findParentVar(Expression e) @safe
    {
        for (;;)
        {
            if (auto ve = e.isVarExp())
            {
                VarDeclaration v = ve.var.isVarDeclaration();
                assert(v);
                return v;
            }
            if (auto ie = e.isIndexExp())
                e = ie.e1;
            else if (auto dve = e.isDotVarExp())
                e = dve.e1;
            else if (auto dtie = e.isDotTemplateInstanceExp())
                e = dtie.e1;
            else if (auto se = e.isSliceExp())
                e = se.e1;
            else
                return null;
        }
    }

    extern (D) private void interpretAssignCommon(BinExp e, fp_t fp, int post = 0)
    {
        debug (LOG)
        {
            printf("%s BinExp::interpretAssignCommon() %s\n", e.loc.toChars(), e.toChars());
        }
        result = CTFEExp.cantexp;

        Expression e1 = e.e1;
        if (!istate)
        {
            error(e.loc, "value of `%s` is not known at compile time", e1.toChars());
            return;
        }

        ++ctfeGlobals.numAssignments;

        /* Before we begin, we need to know if this is a reference assignment
         * (dynamic array, AA, or class) or a value assignment.
         * Determining this for slice assignments are tricky: we need to know
         * if it is a block assignment (a[] = e) rather than a direct slice
         * assignment (a[] = b[]). Note that initializers of multi-dimensional
         * static arrays can have 2D block assignments (eg, int[7][7] x = 6;).
         * So we need to recurse to determine if it is a block assignment.
         */
        bool isBlockAssignment = false;
        if (e1.op == EXP.slice)
        {
            // a[] = e can have const e. So we compare the naked types.
            Type tdst = e1.type.toBasetype();
            Type tsrc = e.e2.type.toBasetype();
            while (tdst.isStaticOrDynamicArray())
            {
                tdst = (cast(TypeArray)tdst).next.toBasetype();
                if (tsrc.equivalent(tdst))
                {
                    isBlockAssignment = true;
                    break;
                }
            }
        }

        // ---------------------------------------
        //      Deal with reference assignment
        // ---------------------------------------
        // If it is a construction of a ref variable, it is a ref assignment
        if ((e.op == EXP.construct || e.op == EXP.blit) &&
            ((cast(AssignExp)e).memset == MemorySet.referenceInit))
        {
            assert(!fp);

            Expression newval = interpretRegion(e.e2, istate, CTFEGoal.LValue);
            if (exceptionOrCant(newval))
                return;

            VarDeclaration v = e1.isVarExp().var.isVarDeclaration();
            setValue(v, newval);

            // Get the value to return. Note that 'newval' is an Lvalue,
            // so if we need an Rvalue, we have to interpret again.
            if (goal == CTFEGoal.RValue)
                result = interpretRegion(newval, istate);
            else
                result = e1; // VarExp is a CTFE reference
            return;
        }

        if (fp)
        {
            while (e1.op == EXP.cast_)
            {
                CastExp ce = e1.isCastExp();
                e1 = ce.e1;
            }
        }

        // ---------------------------------------
        //      Interpret left hand side
        // ---------------------------------------
        AssocArrayLiteralExp existingAA = null;
        Expression lastIndex = null;
        Expression oldval = null;
        if (e1.op == EXP.index && e1.isIndexExp().e1.type.toBasetype().ty == Taarray)
        {
            // ---------------------------------------
            //      Deal with AA index assignment
            // ---------------------------------------
            /* This needs special treatment if the AA doesn't exist yet.
             * There are two special cases:
             * (1) If the AA is itself an index of another AA, we may need to create
             *     multiple nested AA literals before we can insert the new value.
             * (2) If the ultimate AA is null, no insertion happens at all. Instead,
             *     we create nested AA literals, and change it into a assignment.
             */
            IndexExp ie = e1.isIndexExp();
            int depth = 0; // how many nested AA indices are there?
            while (ie.e1.op == EXP.index && ie.e1.isIndexExp().e1.type.toBasetype().ty == Taarray)
            {
                assert(ie.modifiable);
                ie = ie.e1.isIndexExp();
                ++depth;
            }

            // Get the AA value to be modified.
            Expression aggregate = interpretRegion(ie.e1, istate);
            if (exceptionOrCant(aggregate))
                return;
            if ((existingAA = aggregate.isAssocArrayLiteralExp()) !is null)
            {
                // Normal case, ultimate parent AA already exists
                // We need to walk from the deepest index up, checking that an AA literal
                // already exists on each level.
                lastIndex = interpretRegion(e1.isIndexExp().e2, istate);
                lastIndex = resolveSlice(lastIndex); // only happens with AA assignment
                if (exceptionOrCant(lastIndex))
                    return;

                while (depth > 0)
                {
                    // Walk the syntax tree to find the indexExp at this depth
                    IndexExp xe = e1.isIndexExp();
                    foreach (d; 0 .. depth)
                        xe = xe.e1.isIndexExp();

                    Expression ekey = interpretRegion(xe.e2, istate);
                    if (exceptionOrCant(ekey))
                        return;
                    UnionExp ekeyTmp = void;
                    ekey = resolveSlice(ekey, &ekeyTmp); // only happens with AA assignment

                    // Look up this index in it up in the existing AA, to get the next level of AA.
                    AssocArrayLiteralExp newAA = cast(AssocArrayLiteralExp)findKeyInAA(e.loc, existingAA, ekey);
                    if (exceptionOrCant(newAA))
                        return;
                    if (!newAA)
                    {
                        // Doesn't exist yet, create an empty AA...
                        auto keysx = new Expressions();
                        auto valuesx = new Expressions();
                        newAA = ctfeEmplaceExp!AssocArrayLiteralExp(e.loc, keysx, valuesx);
                        newAA.type = xe.type;
                        newAA.ownedByCtfe = OwnedBy.ctfe;
                        //... and insert it into the existing AA.
                        existingAA.keys.push(ekey);
                        existingAA.values.push(newAA);
                    }
                    existingAA = newAA;
                    --depth;
                }

                if (fp)
                {
                    oldval = findKeyInAA(e.loc, existingAA, lastIndex);
                    if (!oldval)
                        oldval = copyLiteral(e.e1.type.defaultInitLiteral(e.loc)).copy();
                }
            }
            else
            {
                /* The AA is currently null. 'aggregate' is actually a reference to
                 * whatever contains it. It could be anything: var, dotvarexp, ...
                 * We rewrite the assignment from:
                 *     aa[i][j] op= newval;
                 * into:
                 *     aa = [i:[j:T.init]];
                 *     aa[j] op= newval;
                 */
                oldval = copyLiteral(e.e1.type.defaultInitLiteral(e.loc)).copy();

                Expression newaae = oldval;
                while (e1.op == EXP.index && e1.isIndexExp().e1.type.toBasetype().ty == Taarray)
                {
                    Expression ekey = interpretRegion(e1.isIndexExp().e2, istate);
                    if (exceptionOrCant(ekey))
                        return;
                    ekey = resolveSlice(ekey); // only happens with AA assignment

                    auto keysx = new Expressions();
                    auto valuesx = new Expressions();
                    keysx.push(ekey);
                    valuesx.push(newaae);

                    auto aae = ctfeEmplaceExp!AssocArrayLiteralExp(e.loc, keysx, valuesx);
                    aae.type = e1.isIndexExp().e1.type;
                    aae.ownedByCtfe = OwnedBy.ctfe;
                    if (!existingAA)
                    {
                        existingAA = aae;
                        lastIndex = ekey;
                    }
                    newaae = aae;
                    e1 = e1.isIndexExp().e1;
                }

                // We must set to aggregate with newaae
                e1 = interpretRegion(e1, istate, CTFEGoal.LValue);
                if (exceptionOrCant(e1))
                    return;
                e1 = assignToLvalue(e, e1, newaae);
                if (exceptionOrCant(e1))
                    return;
            }
            assert(existingAA && lastIndex);
            e1 = null; // stomp
        }
        else if (e1.op == EXP.arrayLength)
        {
            oldval = interpretRegion(e1, istate);
            if (exceptionOrCant(oldval))
                return;
        }
        else if (e.op == EXP.construct || e.op == EXP.blit)
        {
            // Unless we have a simple var assignment, we're
            // only modifying part of the variable. So we need to make sure
            // that the parent variable exists.
            VarDeclaration ultimateVar = findParentVar(e1);
            if (auto ve = e1.isVarExp())
            {
                VarDeclaration v = ve.var.isVarDeclaration();
                assert(v);
                if (v.storage_class & STC.out_)
                    goto L1;
            }
            else if (ultimateVar && !getValue(ultimateVar))
            {
                Expression ex = interpretRegion(ultimateVar.type.defaultInitLiteral(e.loc), istate);
                if (exceptionOrCant(ex))
                    return;
                setValue(ultimateVar, ex);
            }
            else
                goto L1;
        }
        else
        {
        L1:
            e1 = interpretRegion(e1, istate, CTFEGoal.LValue);
            if (exceptionOrCant(e1))
                return;

            if (e1.op == EXP.index && e1.isIndexExp().e1.type.toBasetype().ty == Taarray)
            {
                IndexExp ie = e1.isIndexExp();
                assert(ie.e1.op == EXP.assocArrayLiteral);
                existingAA = ie.e1.isAssocArrayLiteralExp();
                lastIndex = ie.e2;
            }
        }

        // ---------------------------------------
        //      Interpret right hand side
        // ---------------------------------------
        Expression newval = interpretRegion(e.e2, istate);
        if (exceptionOrCant(newval))
            return;
        if (e.op == EXP.blit && newval.op == EXP.int64)
        {
            Type tbn = e.type.baseElemOf();
            if (tbn.ty == Tstruct)
            {
                /* Look for special case of struct being initialized with 0.
                 */
                newval = e.type.defaultInitLiteral(e.loc);
                if (newval.op == EXP.error)
                {
                    result = CTFEExp.cantexp;
                    return;
                }
                newval = interpretRegion(newval, istate); // copy and set ownedByCtfe flag
                if (exceptionOrCant(newval))
                    return;
            }
        }

        // ----------------------------------------------------
        //  Deal with read-modify-write assignments.
        //  Set 'newval' to the final assignment value
        //  Also determine the return value (except for slice
        //  assignments, which are more complicated)
        // ----------------------------------------------------
        if (fp)
        {
            if (!oldval)
            {
                // Load the left hand side after interpreting the right hand side.
                oldval = interpretRegion(e1, istate);
                if (exceptionOrCant(oldval))
                    return;
            }

            if (e.e1.type.ty != Tpointer)
            {
                // ~= can create new values (see bug 6052)
                if (e.op == EXP.concatenateAssign || e.op == EXP.concatenateElemAssign || e.op == EXP.concatenateDcharAssign)
                {
                    // We need to dup it and repaint the type. For a dynamic array
                    // we can skip duplication, because it gets copied later anyway.
                    if (newval.type.ty != Tarray)
                    {
                        newval = copyLiteral(newval).copy();
                        newval.type = e.e2.type; // repaint type
                    }
                    else
                    {
                        newval = paintTypeOntoLiteral(e.e2.type, newval);
                        newval = resolveSlice(newval);
                    }
                }
                oldval = resolveSlice(oldval);

                newval = (*fp)(e.loc, e.type, oldval, newval).copy();
            }
            else if (e.e2.type.isIntegral() &&
                     (e.op == EXP.addAssign ||
                      e.op == EXP.minAssign ||
                      e.op == EXP.plusPlus ||
                      e.op == EXP.minusMinus))
            {
                newval = pointerArithmetic(pue, e.loc, e.op, e.type, oldval, newval).copy();
                if (newval == pue.exp())
                    newval = pue.copy();
            }
            else
            {
                error(e.loc, "pointer expression `%s` cannot be interpreted at compile time", e.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            if (exceptionOrCant(newval))
            {
                if (CTFEExp.isCantExp(newval))
                    error(e.loc, "cannot interpret `%s` at compile time", e.toChars());
                return;
            }
        }

        if (existingAA)
        {
            if (existingAA.ownedByCtfe != OwnedBy.ctfe)
            {
                error(e.loc, "cannot modify read-only constant `%s`", existingAA.toChars());
                result = CTFEExp.cantexp;
                return;
            }

            //printf("\t+L%d existingAA = %s, lastIndex = %s, oldval = %s, newval = %s\n",
            //    __LINE__, existingAA.toChars(), lastIndex.toChars(), oldval ? oldval.toChars() : NULL, newval.toChars());
            assignAssocArrayElement(e.loc, existingAA, lastIndex, newval);

            // Determine the return value
            result = ctfeCast(pue, e.loc, e.type, e.type, fp && post ? oldval : newval);
            return;
        }
        if (e1.op == EXP.arrayLength)
        {
            /* Change the assignment from:
             *  arr.length = n;
             * into:
             *  arr = new_length_array; (result is n)
             */

            // Determine the return value
            result = ctfeCast(pue, e.loc, e.type, e.type, fp && post ? oldval : newval);
            if (exceptionOrCant(result))
                return;

            if (result == pue.exp())
                result = pue.copy();

            size_t oldlen = cast(size_t)oldval.toInteger();
            size_t newlen = cast(size_t)newval.toInteger();
            if (oldlen == newlen) // no change required -- we're done!
                return;

            // We have changed it into a reference assignment
            // Note that returnValue is still the new length.
            e1 = e1.isArrayLengthExp().e1;
            Type t = e1.type.toBasetype();
            if (t.ty != Tarray)
            {
                error(e.loc, "`%s` is not yet supported at compile time", e.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            e1 = interpretRegion(e1, istate, CTFEGoal.LValue);
            if (exceptionOrCant(e1))
                return;

            if (oldlen != 0) // Get the old array literal.
                oldval = interpretRegion(e1, istate);
            UnionExp utmp = void;
            oldval = resolveSlice(oldval, &utmp);

            newval = changeArrayLiteralLength(pue, e.loc, cast(TypeArray)t, oldval, oldlen, newlen);
            if (newval == pue.exp())
                newval = pue.copy();

            e1 = assignToLvalue(e, e1, newval);
            if (exceptionOrCant(e1))
                return;

            return;
        }

        if (!isBlockAssignment)
        {
            newval = ctfeCast(pue, e.loc, e.type, e.type, newval);
            if (exceptionOrCant(newval))
                return;
            if (newval == pue.exp())
                newval = pue.copy();

            // Determine the return value
            if (goal == CTFEGoal.LValue) // https://issues.dlang.org/show_bug.cgi?id=14371
                result = e1;
            else
            {
                result = ctfeCast(pue, e.loc, e.type, e.type, fp && post ? oldval : newval);
                if (result == pue.exp())
                    result = pue.copy();
            }
            if (exceptionOrCant(result))
                return;
        }
        if (exceptionOrCant(newval))
            return;

        debug (LOGASSIGN)
        {
            printf("ASSIGN: %s=%s\n", e1.toChars(), newval.toChars());
            showCtfeExpr(newval);
        }

        /* Block assignment or element-wise assignment.
         */
        if (e1.op == EXP.slice ||
            e1.op == EXP.vector ||
            e1.op == EXP.arrayLiteral ||
            e1.op == EXP.string_ ||
            e1.op == EXP.null_ && e1.type.toBasetype().ty == Tarray)
        {
            // Note that slice assignments don't support things like ++, so
            // we don't need to remember 'returnValue'.
            result = interpretAssignToSlice(pue, e, e1, newval, isBlockAssignment);
            if (exceptionOrCant(result))
                return;
            if (auto se = e.e1.isSliceExp())
            {
                Expression e1x = interpretRegion(se.e1, istate, CTFEGoal.LValue);
                if (auto dve = e1x.isDotVarExp())
                {
                    auto ex = dve.e1;
                    auto sle = ex.op == EXP.structLiteral ? ex.isStructLiteralExp()
                             : ex.op == EXP.classReference ? ex.isClassReferenceExp().value
                             : null;
                    auto v = dve.var.isVarDeclaration();
                    if (!sle || !v)
                    {
                        error(e.loc, "CTFE internal error: dotvar slice assignment");
                        result = CTFEExp.cantexp;
                        return;
                    }
                    stompOverlappedFields(sle, v);
                }
            }
            return;
        }
        assert(result);

        /* Assignment to a CTFE reference.
         */
        if (Expression ex = assignToLvalue(e, e1, newval))
            result = ex;

        return;
    }

    /* Set all sibling fields which overlap with v to VoidExp.
     */
    private void stompOverlappedFields(StructLiteralExp sle, VarDeclaration v)
    {
        if (!v.overlapped)
            return;
        foreach (size_t i, v2; sle.sd.fields)
        {
            if (v is v2 || !v.isOverlappedWith(v2))
                continue;
            auto e = (*sle.elements)[i];
            if (e !is null && e.op != EXP.void_)
                (*sle.elements)[i] = voidInitLiteral(e.type, v).copy();
        }
    }

    private Expression assignToLvalue(BinExp e, Expression e1, Expression newval)
    {
        //printf("assignToLvalue() e: %s e1: %s newval: %s\n", e.toChars(), e1.toChars(), newval.toChars());
        VarDeclaration vd = null;
        Expression* payload = null; // dead-store to prevent spurious warning
        Expression oldval;

        if (auto ve = e1.isVarExp())
        {
            vd = ve.var.isVarDeclaration();
            oldval = getValue(vd);
        }
        else if (auto dve = e1.isDotVarExp())
        {
            /* Assignment to member variable of the form:
             *  e.v = newval
             */
            auto ex = dve.e1;
            auto sle = ex.op == EXP.structLiteral ? ex.isStructLiteralExp()
                     : ex.op == EXP.classReference ? ex.isClassReferenceExp().value
                     : null;
            auto v = e1.isDotVarExp().var.isVarDeclaration();
            if (!sle || !v)
            {
                error(e.loc, "CTFE internal error: dotvar assignment");
                return CTFEExp.cantexp;
            }
            if (sle.ownedByCtfe != OwnedBy.ctfe)
            {
                error(e.loc, "cannot modify read-only constant `%s`", sle.toChars());
                return CTFEExp.cantexp;
            }

            int fieldi = ex.op == EXP.structLiteral ? findFieldIndexByName(sle.sd, v)
                       : ex.isClassReferenceExp().findFieldIndexByName(v);
            if (fieldi == -1)
            {
                error(e.loc, "CTFE internal error: cannot find field `%s` in `%s`", v.toChars(), ex.toChars());
                return CTFEExp.cantexp;
            }
            assert(0 <= fieldi && fieldi < sle.elements.length);

            // If it's a union, set all other members of this union to void
            stompOverlappedFields(sle, v);

            payload = &(*sle.elements)[fieldi];
            oldval = *payload;
            if (auto ival = newval.isIntegerExp())
            {
                if (auto bf = v.isBitFieldDeclaration())
                {
                    sinteger_t value = ival.toInteger();
                    if (bf.type.isUnsigned())
                        value &= (1L << bf.fieldWidth) - 1; // zero extra bits
                    else
                    {   // sign extend extra bits
                        value = value << (64 - bf.fieldWidth);
                        value = value >> (64 - bf.fieldWidth);
                    }
                    ival.setInteger(value);
                }
            }
        }
        else if (auto ie = e1.isIndexExp())
        {
            assert(ie.e1.type.toBasetype().ty != Taarray);

            Expression aggregate;
            uinteger_t indexToModify;
            if (!resolveIndexing(ie, istate, &aggregate, &indexToModify, true))
            {
                return CTFEExp.cantexp;
            }
            size_t index = cast(size_t)indexToModify;

            if (auto existingSE = aggregate.isStringExp())
            {
                if (existingSE.ownedByCtfe != OwnedBy.ctfe)
                {
                    error(e.loc, "cannot modify read-only string literal `%s`", ie.e1.toChars());
                    return CTFEExp.cantexp;
                }
                existingSE.setCodeUnit(index, cast(dchar)newval.toInteger());
                return null;
            }
            if (aggregate.op != EXP.arrayLiteral)
            {
                error(e.loc, "index assignment `%s` is not yet supported in CTFE ", e.toChars());
                return CTFEExp.cantexp;
            }

            ArrayLiteralExp existingAE = aggregate.isArrayLiteralExp();
            if (existingAE.ownedByCtfe != OwnedBy.ctfe)
            {
                error(e.loc, "cannot modify read-only constant `%s`", existingAE.toChars());
                return CTFEExp.cantexp;
            }

            payload = &(*existingAE.elements)[index];
            oldval = *payload;
        }
        else
        {
            error(e.loc, "`%s` cannot be evaluated at compile time", e.toChars());
            return CTFEExp.cantexp;
        }

        Type t1b = e1.type.toBasetype();
        bool wantCopy = t1b.baseElemOf().ty == Tstruct;

        if (auto ve = newval.isVectorExp())
        {
            // Ensure ve is an array literal, and not a broadcast
            if (ve.e1.op == EXP.int64 || ve.e1.op == EXP.float64) // if broadcast
            {
                UnionExp ue = void;
                Expression ex = interpretVectorToArray(&ue, ve);
                ve.e1 = (ex == ue.exp()) ? ue.copy() : ex;
            }
        }

        if (newval.op == EXP.structLiteral && oldval)
        {
            assert(oldval.op == EXP.structLiteral || oldval.op == EXP.arrayLiteral || oldval.op == EXP.string_);
            newval = copyLiteral(newval).copy();
            assignInPlace(oldval, newval);
        }
        else if (wantCopy && (e.op == EXP.assign || e.op == EXP.loweredAssignExp))
        {
            // Currently postblit/destructor calls on static array are done
            // in the druntime internal functions so they don't appear in AST.
            // Therefore interpreter should handle them specially.

            assert(oldval);
            version (all) // todo: instead we can directly access to each elements of the slice
            {
                newval = resolveSlice(newval);
                if (CTFEExp.isCantExp(newval))
                {
                    error(e.loc, "CTFE internal error: assignment `%s`", e.toChars());
                    return CTFEExp.cantexp;
                }
            }
            assert(oldval.op == EXP.arrayLiteral);
            assert(newval.op == EXP.arrayLiteral);

            Expressions* oldelems = oldval.isArrayLiteralExp().elements;
            Expressions* newelems = newval.isArrayLiteralExp().elements;
            assert(oldelems.length == newelems.length);

            Type elemtype = oldval.type.nextOf();
            foreach (i, ref oldelem; *oldelems)
            {
                Expression newelem = paintTypeOntoLiteral(elemtype, (*newelems)[i]);
                // https://issues.dlang.org/show_bug.cgi?id=9245
                if (e.e2.isLvalue())
                {
                    if (Expression ex = evaluatePostblit(istate, newelem))
                        return ex;
                }
                // https://issues.dlang.org/show_bug.cgi?id=13661
                if (Expression ex = evaluateDtor(istate, oldelem))
                    return ex;
                oldelem = newelem;
            }
        }
        else
        {
            // e1 has its own payload, so we have to create a new literal.
            if (wantCopy)
                newval = copyLiteral(newval).copy();

            if (t1b.ty == Tsarray && e.op == EXP.construct && e.e2.isLvalue())
            {
                // https://issues.dlang.org/show_bug.cgi?id=9245
                if (Expression ex = evaluatePostblit(istate, newval))
                    return ex;
            }

            oldval = newval;
        }

        if (vd)
            setValue(vd, oldval);
        else
            *payload = oldval;

        // Blit assignment should return the newly created value.
        if (e.op == EXP.blit)
            return oldval;

        return null;
    }

    /*************
     * Deal with assignments of the form:
     *  dest[] = newval
     *  dest[low..upp] = newval
     * where newval has already been interpreted
     *
     * This could be a slice assignment or a block assignment, and
     * dest could be either an array literal, or a string.
     *
     * Returns EXP.cantExpression on failure. If there are no errors,
     * it returns aggregate[low..upp], except that as an optimisation,
     * if goal == CTFEGoal.Nothing, it will return NULL
     */
    private Expression interpretAssignToSlice(UnionExp* pue, BinExp e, Expression e1, Expression newval, bool isBlockAssignment)
    {
        //printf("interpretAssignToSlice(e: %s e1: %s newval: %s\n", e.toChars(), e1.toChars(), newval.toChars());

        dinteger_t lowerbound;
        dinteger_t upperbound;
        dinteger_t firstIndex;

        Expression aggregate;

        if (auto se = e1.isSliceExp())
        {
            // ------------------------------
            //   aggregate[] = newval
            //   aggregate[low..upp] = newval
            // ------------------------------
            aggregate = interpretRegion(se.e1, istate);
            lowerbound = se.lwr ? se.lwr.toInteger() : 0;
            upperbound = se.upr ? se.upr.toInteger() : resolveArrayLength(aggregate);

            // Slice of a slice --> change the bounds
            if (auto oldse = aggregate.isSliceExp())
            {
                aggregate = oldse.e1;
                firstIndex = lowerbound + oldse.lwr.toInteger();
            }
            else
                firstIndex = lowerbound;
        }
        else
        {
            if (auto ale = e1.isArrayLiteralExp())
            {
                lowerbound = 0;
                upperbound = ale.elements.length;
            }
            else if (auto se = e1.isStringExp())
            {
                lowerbound = 0;
                upperbound = se.len;
            }
            else if (e1.op == EXP.null_)
            {
                lowerbound = 0;
                upperbound = 0;
            }
            else if (VectorExp ve = e1.isVectorExp())
            {
                // ve is not handled but a proper error message is returned
                // this is to prevent https://issues.dlang.org/show_bug.cgi?id=20042
                lowerbound = 0;
                upperbound = ve.dim;
            }
            else
                assert(0);

            aggregate = e1;
            firstIndex = lowerbound;
        }
        if (upperbound == lowerbound)
            return newval;

        // For slice assignment, we check that the lengths match.
        if (!isBlockAssignment && e1.type.ty != Tpointer)
        {
            const srclen = resolveArrayLength(newval);
            if (srclen != (upperbound - lowerbound))
            {
                error(e.loc, "array length mismatch assigning `[0..%llu]` to `[%llu..%llu]`",
                    ulong(srclen), ulong(lowerbound), ulong(upperbound));
                return CTFEExp.cantexp;
            }
        }

        if (auto existingSE = aggregate.isStringExp())
        {
            if (existingSE.ownedByCtfe != OwnedBy.ctfe)
            {
                error(e.loc, "cannot modify read-only string literal `%s`", existingSE.toChars());
                return CTFEExp.cantexp;
            }

            if (auto se = newval.isSliceExp())
            {
                auto aggr2 = se.e1;
                const srclower = se.lwr.toInteger();
                const srcupper = se.upr.toInteger();

                if (aggregate == aggr2 &&
                    lowerbound < srcupper && srclower < upperbound)
                {
                    error(e.loc, "overlapping slice assignment `[%llu..%llu] = [%llu..%llu]`",
                        ulong(lowerbound), ulong(upperbound), ulong(srclower), ulong(srcupper));
                    return CTFEExp.cantexp;
                }
                version (all) // todo: instead we can directly access to each elements of the slice
                {
                    Expression orignewval = newval;
                    newval = resolveSlice(newval);
                    if (CTFEExp.isCantExp(newval))
                    {
                        error(e.loc, "CTFE internal error: slice `%s`", orignewval.toChars());
                        return CTFEExp.cantexp;
                    }
                }
                assert(newval.op != EXP.slice);
            }
            if (auto se = newval.isStringExp())
            {
                sliceAssignStringFromString(existingSE, se, cast(size_t)firstIndex);
                return newval;
            }
            if (auto ale = newval.isArrayLiteralExp())
            {
                /* Mixed slice: it was initialized as a string literal.
                 * Now a slice of it is being set with an array literal.
                 */
                sliceAssignStringFromArrayLiteral(existingSE, ale, cast(size_t)firstIndex);
                return newval;
            }

            // String literal block slice assign
            const value = cast(dchar)newval.toInteger();
            foreach (i; 0 .. upperbound - lowerbound)
            {
                existingSE.setCodeUnit(cast(size_t)(i + firstIndex), value);
            }
            if (goal == CTFEGoal.Nothing)
                return null; // avoid creating an unused literal
            auto retslice = ctfeEmplaceExp!SliceExp(e.loc, existingSE,
                        ctfeEmplaceExp!IntegerExp(e.loc, firstIndex, Type.tsize_t),
                        ctfeEmplaceExp!IntegerExp(e.loc, firstIndex + upperbound - lowerbound, Type.tsize_t));
            retslice.type = e.type;
            return interpret(pue, retslice, istate);
        }
        if (auto existingAE = aggregate.isArrayLiteralExp())
        {
            if (existingAE.ownedByCtfe != OwnedBy.ctfe)
            {
                error(e.loc, "cannot modify read-only constant `%s`", existingAE.toChars());
                return CTFEExp.cantexp;
            }

            if (newval.op == EXP.slice && !isBlockAssignment)
            {
                auto se = newval.isSliceExp();
                auto aggr2 = se.e1;
                const srclower = se.lwr.toInteger();
                const srcupper = se.upr.toInteger();
                const wantCopy = (newval.type.toBasetype().nextOf().baseElemOf().ty == Tstruct);

                //printf("oldval = %p %s[%d..%u]\nnewval = %p %s[%llu..%llu] wantCopy = %d\n",
                //    aggregate, aggregate.toChars(), lowerbound, upperbound,
                //    aggr2, aggr2.toChars(), srclower, srcupper, wantCopy);
                if (wantCopy)
                {
                    // Currently overlapping for struct array is allowed.
                    // The order of elements processing depends on the overlapping.
                    // https://issues.dlang.org/show_bug.cgi?id=14024
                    assert(aggr2.op == EXP.arrayLiteral);
                    Expressions* oldelems = existingAE.elements;
                    Expressions* newelems = aggr2.isArrayLiteralExp().elements;

                    Type elemtype = aggregate.type.nextOf();
                    bool needsPostblit = e.e2.isLvalue();

                    if (aggregate == aggr2 && srclower < lowerbound && lowerbound < srcupper)
                    {
                        // reverse order
                        for (auto i = upperbound - lowerbound; 0 < i--;)
                        {
                            Expression oldelem = (*oldelems)[cast(size_t)(i + firstIndex)];
                            Expression newelem = (*newelems)[cast(size_t)(i + srclower)];
                            newelem = copyLiteral(newelem).copy();
                            newelem.type = elemtype;
                            if (needsPostblit)
                            {
                                if (Expression x = evaluatePostblit(istate, newelem))
                                    return x;
                            }
                            if (Expression x = evaluateDtor(istate, oldelem))
                                return x;
                            (*oldelems)[cast(size_t)(lowerbound + i)] = newelem;
                        }
                    }
                    else
                    {
                        // normal order
                        for (auto i = 0; i < upperbound - lowerbound; i++)
                        {
                            Expression oldelem = (*oldelems)[cast(size_t)(i + firstIndex)];
                            Expression newelem = (*newelems)[cast(size_t)(i + srclower)];
                            newelem = copyLiteral(newelem).copy();
                            newelem.type = elemtype;
                            if (needsPostblit)
                            {
                                if (Expression x = evaluatePostblit(istate, newelem))
                                    return x;
                            }
                            if (Expression x = evaluateDtor(istate, oldelem))
                                return x;
                            (*oldelems)[cast(size_t)(lowerbound + i)] = newelem;
                        }
                    }

                    //assert(0);
                    return newval; // oldval?
                }
                if (aggregate == aggr2 &&
                    lowerbound < srcupper && srclower < upperbound)
                {
                    error(e.loc, "overlapping slice assignment `[%llu..%llu] = [%llu..%llu]`",
                        ulong(lowerbound), ulong(upperbound), ulong(srclower), ulong(srcupper));
                    return CTFEExp.cantexp;
                }
                version (all) // todo: instead we can directly access to each elements of the slice
                {
                    Expression orignewval = newval;
                    newval = resolveSlice(newval);
                    if (CTFEExp.isCantExp(newval))
                    {
                        error(e.loc, "CTFE internal error: slice `%s`", orignewval.toChars());
                        return CTFEExp.cantexp;
                    }
                }
                // no overlapping
                //length?
                assert(newval.op != EXP.slice);
            }
            if (newval.op == EXP.string_ && !isBlockAssignment)
            {
                /* Mixed slice: it was initialized as an array literal of chars/integers.
                 * Now a slice of it is being set with a string.
                 */
                sliceAssignArrayLiteralFromString(existingAE, newval.isStringExp(), cast(size_t)firstIndex);
                return newval;
            }
            if (newval.op == EXP.arrayLiteral && !isBlockAssignment)
            {
                Expressions* oldelems = existingAE.elements;
                Expressions* newelems = newval.isArrayLiteralExp().elements;
                Type elemtype = existingAE.type.nextOf();
                bool needsPostblit = e.op != EXP.blit && e.e2.isLvalue();
                foreach (j, newelem; *newelems)
                {
                    newelem = paintTypeOntoLiteral(elemtype, newelem);
                    if (needsPostblit)
                    {
                        Expression x = evaluatePostblit(istate, newelem);
                        if (exceptionOrCantInterpret(x))
                            return x;
                    }
                    (*oldelems)[cast(size_t)(j + firstIndex)] = newelem;
                }
                return newval;
            }

            /* Block assignment, initialization of static arrays
             *   x[] = newval
             *  x may be a multidimensional static array. (Note that this
             *  only happens with array literals, never with strings).
             */
            struct RecursiveBlock
            {
                InterState* istate;
                Expression newval;
                bool refCopy;
                bool needsPostblit;
                bool needsDtor;

                Expression assignTo(ArrayLiteralExp ae)
                {
                    return assignTo(ae, 0, ae.elements.length);
                }

                Expression assignTo(ArrayLiteralExp ae, size_t lwr, size_t upr)
                {
                    Expressions* w = ae.elements;
                    assert(ae.type.isStaticOrDynamicArray() || ae.type.ty == Tpointer);
                    bool directblk = (cast(TypeNext)ae.type).next.equivalent(newval.type);
                    for (size_t k = lwr; k < upr; k++)
                    {
                        if (!directblk && (*w)[k].op == EXP.arrayLiteral)
                        {
                            // Multidimensional array block assign
                            if (Expression ex = assignTo((*w)[k].isArrayLiteralExp()))
                                return ex;
                        }
                        else if (refCopy)
                        {
                            (*w)[k] = newval;
                        }
                        else if (!needsPostblit && !needsDtor)
                        {
                            assignInPlace((*w)[k], newval);
                        }
                        else
                        {
                            Expression oldelem = (*w)[k];
                            Expression tmpelem = needsDtor ? copyLiteral(oldelem).copy() : null;
                            assignInPlace(oldelem, newval);
                            if (needsPostblit)
                            {
                                if (Expression ex = evaluatePostblit(istate, oldelem))
                                    return ex;
                            }
                            if (needsDtor)
                            {
                                // https://issues.dlang.org/show_bug.cgi?id=14860
                                if (Expression ex = evaluateDtor(istate, tmpelem))
                                    return ex;
                            }
                        }
                    }
                    return null;
                }
            }

            Type tn = newval.type.toBasetype();
            bool wantRef = (tn.ty == Tarray || isAssocArray(tn) || tn.ty == Tclass);
            bool cow = newval.op != EXP.structLiteral && newval.op != EXP.arrayLiteral && newval.op != EXP.string_;
            Type tb = tn.baseElemOf();
            StructDeclaration sd = (tb.ty == Tstruct ? (cast(TypeStruct)tb).sym : null);

            RecursiveBlock rb;
            rb.istate = istate;
            rb.newval = newval;
            rb.refCopy = wantRef || cow;
            rb.needsPostblit = sd && sd.postblit && e.op != EXP.blit && e.e2.isLvalue();
            rb.needsDtor = sd && sd.dtor && (e.op == EXP.assign || e.op == EXP.loweredAssignExp);
            if (Expression ex = rb.assignTo(existingAE, cast(size_t)lowerbound, cast(size_t)upperbound))
                return ex;

            if (goal == CTFEGoal.Nothing)
                return null; // avoid creating an unused literal
            auto retslice = ctfeEmplaceExp!SliceExp(e.loc, existingAE,
                ctfeEmplaceExp!IntegerExp(e.loc, firstIndex, Type.tsize_t),
                ctfeEmplaceExp!IntegerExp(e.loc, firstIndex + upperbound - lowerbound, Type.tsize_t));
            retslice.type = e.type;
            return interpret(pue, retslice, istate);
        }

        error(e.loc, "slice operation `%s = %s` cannot be evaluated at compile time", e1.toChars(), newval.toChars());
        return CTFEExp.cantexp;
    }

    override void visit(AssignExp e)
    {
        interpretAssignCommon(e, null);
    }

    override void visit(BinAssignExp e)
    {
        switch (e.op)
        {
        case EXP.addAssign:
            interpretAssignCommon(e, &Add);
            return;

        case EXP.minAssign:
            interpretAssignCommon(e, &Min);
            return;

        case EXP.concatenateAssign:
        case EXP.concatenateElemAssign:
        case EXP.concatenateDcharAssign:
            interpretAssignCommon(e, &ctfeCat);
            return;

        case EXP.mulAssign:
            interpretAssignCommon(e, &Mul);
            return;

        case EXP.divAssign:
            interpretAssignCommon(e, &Div);
            return;

        case EXP.modAssign:
            interpretAssignCommon(e, &Mod);
            return;

        case EXP.leftShiftAssign:
            interpretAssignCommon(e, &Shl);
            return;

        case EXP.rightShiftAssign:
            interpretAssignCommon(e, &Shr);
            return;

        case EXP.unsignedRightShiftAssign:
            interpretAssignCommon(e, &Ushr);
            return;

        case EXP.andAssign:
            interpretAssignCommon(e, &And);
            return;

        case EXP.orAssign:
            interpretAssignCommon(e, &Or);
            return;

        case EXP.xorAssign:
            interpretAssignCommon(e, &Xor);
            return;

        case EXP.powAssign:
            interpretAssignCommon(e, &Pow);
            return;

        default:
            assert(0);
        }
    }

    override void visit(PostExp e)
    {
        debug (LOG)
        {
            printf("%s PostExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.op == EXP.plusPlus)
            interpretAssignCommon(e, &Add, 1);
        else
            interpretAssignCommon(e, &Min, 1);
        debug (LOG)
        {
            if (CTFEExp.isCantExp(result))
                printf("PostExp::interpret() CANT\n");
        }
    }

    /* Return 1 if e is a p1 > p2 or p1 >= p2 pointer comparison;
     *       -1 if e is a p1 < p2 or p1 <= p2 pointer comparison;
     *        0 otherwise
     */
    static int isPointerCmpExp(Expression e, Expression* p1, Expression* p2)
    {
        int ret = 1;
        while (e.op == EXP.not)
        {
            ret *= -1;
            e = e.isNotExp().e1;
        }
        switch (e.op)
        {
        case EXP.lessThan:
        case EXP.lessOrEqual:
            ret *= -1;
            goto case; /+ fall through +/
        case EXP.greaterThan:
        case EXP.greaterOrEqual:
            *p1 = e.isBinExp().e1;
            *p2 = e.isBinExp().e2;
            if (!(isPointer((*p1).type) && isPointer((*p2).type)))
                ret = 0;
            break;

        default:
            ret = 0;
            break;
        }
        return ret;
    }

    /** If this is a four pointer relation, evaluate it, else return NULL.
     *
     *  This is an expression of the form (p1 > q1 && p2 < q2) or (p1 < q1 || p2 > q2)
     *  where p1, p2 are expressions yielding pointers to memory block p,
     *  and q1, q2 are expressions yielding pointers to memory block q.
     *  This expression is valid even if p and q are independent memory
     *  blocks and are therefore not normally comparable; the && form returns true
     *  if [p1..p2] lies inside [q1..q2], and false otherwise; the || form returns
     *  true if [p1..p2] lies outside [q1..q2], and false otherwise.
     *
     *  Within the expression, any ordering of p1, p2, q1, q2 is permissible;
     *  the comparison operators can be any of >, <, <=, >=, provided that
     *  both directions (p > q and p < q) are checked. Additionally the
     *  relational sub-expressions can be negated, eg
     *  (!(q1 < p1) && p2 <= q2) is valid.
     */
    private void interpretFourPointerRelation(UnionExp* pue, BinExp e)
    {
        assert(e.op == EXP.andAnd || e.op == EXP.orOr);

        /*  It can only be an isInside expression, if both e1 and e2 are
         *  directional pointer comparisons.
         *  Note that this check can be made statically; it does not depends on
         *  any runtime values. This allows a JIT implementation to compile a
         *  special AndAndPossiblyInside, keeping the normal AndAnd case efficient.
         */

        // Save the pointer expressions and the comparison directions,
        // so we can use them later.
        Expression p1 = null;
        Expression p2 = null;
        Expression p3 = null;
        Expression p4 = null;
        int dir1 = isPointerCmpExp(e.e1, &p1, &p2);
        int dir2 = isPointerCmpExp(e.e2, &p3, &p4);
        if (dir1 == 0 || dir2 == 0)
        {
            result = null;
            return;
        }

        //printf("FourPointerRelation %s\n", toChars());

        UnionExp ue1 = void;
        UnionExp ue2 = void;
        UnionExp ue3 = void;
        UnionExp ue4 = void;

        // Evaluate the first two pointers
        p1 = interpret(&ue1, p1, istate);
        if (exceptionOrCant(p1))
            return;
        p2 = interpret(&ue2, p2, istate);
        if (exceptionOrCant(p2))
            return;
        dinteger_t ofs1, ofs2;
        Expression agg1 = getAggregateFromPointer(p1, &ofs1);
        Expression agg2 = getAggregateFromPointer(p2, &ofs2);

        if (!pointToSameMemoryBlock(agg1, agg2) && agg1.op != EXP.null_ && agg2.op != EXP.null_)
        {
            // Here it is either CANT_INTERPRET,
            // or an IsInside comparison returning false.
            p3 = interpret(&ue3, p3, istate);
            if (CTFEExp.isCantExp(p3))
                return;
            // Note that it is NOT legal for it to throw an exception!
            Expression except = null;
            if (exceptionOrCantInterpret(p3))
                except = p3;
            else
            {
                p4 = interpret(&ue4, p4, istate);
                if (CTFEExp.isCantExp(p4))
                {
                    result = p4;
                    return;
                }
                if (exceptionOrCantInterpret(p4))
                    except = p4;
            }
            if (except)
            {
                error(e.loc, "comparison `%s` of pointers to unrelated memory blocks remains indeterminate at compile time because exception `%s` was thrown while evaluating `%s`", e.e1.toChars(), except.toChars(), e.e2.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            dinteger_t ofs3, ofs4;
            Expression agg3 = getAggregateFromPointer(p3, &ofs3);
            Expression agg4 = getAggregateFromPointer(p4, &ofs4);
            // The valid cases are:
            // p1 > p2 && p3 > p4  (same direction, also for < && <)
            // p1 > p2 && p3 < p4  (different direction, also < && >)
            // Changing any > into >= doesn't affect the result
            if ((dir1 == dir2 && pointToSameMemoryBlock(agg1, agg4) && pointToSameMemoryBlock(agg2, agg3)) ||
                (dir1 != dir2 && pointToSameMemoryBlock(agg1, agg3) && pointToSameMemoryBlock(agg2, agg4)))
            {
                // it's a legal two-sided comparison
                emplaceExp!(IntegerExp)(pue, e.loc, (e.op == EXP.andAnd) ? 0 : 1, e.type);
                result = pue.exp();
                return;
            }
            // It's an invalid four-pointer comparison. Either the second
            // comparison is in the same direction as the first, or else
            // more than two memory blocks are involved (either two independent
            // invalid comparisons are present, or else agg3 == agg4).
            error(e.loc, "comparison `%s` of pointers to unrelated memory blocks is indeterminate at compile time, even when combined with `%s`.", e.e1.toChars(), e.e2.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        // The first pointer expression didn't need special treatment, so we
        // we need to interpret the entire expression exactly as a normal && or ||.
        // This is easy because we haven't evaluated e2 at all yet, and we already
        // know it will return a bool.
        // But we mustn't evaluate the pointer expressions in e1 again, in case
        // they have side-effects.
        bool nott = false;
        Expression ex = e.e1;
        while (1)
        {
            if (auto ne = ex.isNotExp())
            {
                nott = !nott;
                ex = ne.e1;
            }
            else
                break;
        }

        /** Negate relational operator, eg >= becomes <
         * Params:
         *      op = comparison operator to negate
         * Returns:
         *      negate operator
         */
        static EXP negateRelation(EXP op) pure
        {
            switch (op)
            {
                case EXP.greaterOrEqual:  op = EXP.lessThan;       break;
                case EXP.greaterThan:     op = EXP.lessOrEqual;    break;
                case EXP.lessOrEqual:     op = EXP.greaterThan;    break;
                case EXP.lessThan:        op = EXP.greaterOrEqual; break;
                default:                  assert(0);
            }
            return op;
        }

        const EXP cmpop = nott ? negateRelation(ex.op) : ex.op;
        const cmp = comparePointers(cmpop, agg1, ofs1, agg2, ofs2);
        // We already know this is a valid comparison.
        assert(cmp >= 0);
        if (e.op == EXP.andAnd && cmp == 1 || e.op == EXP.orOr && cmp == 0)
        {
            result = interpret(pue, e.e2, istate);
            return;
        }
        emplaceExp!(IntegerExp)(pue, e.loc, (e.op == EXP.andAnd) ? 0 : 1, e.type);
        result = pue.exp();
    }

    override void visit(LogicalExp e)
    {
        debug (LOG)
        {
            printf("%s LogicalExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        // Check for an insidePointer expression, evaluate it if so
        interpretFourPointerRelation(pue, e);
        if (result)
            return;

        UnionExp ue1 = void;
        result = interpret(&ue1, e.e1, istate);
        if (exceptionOrCant(result))
            return;

        bool res;
        const andand = e.op == EXP.andAnd;
        if (andand ? result.toBool().hasValue(false) : isTrueBool(result))
            res = !andand;
        else if (andand ? isTrueBool(result) : result.toBool().hasValue(false))
        {
            UnionExp ue2 = void;
            result = interpret(&ue2, e.e2, istate);
            if (exceptionOrCant(result))
                return;
            if (result.op == EXP.voidExpression)
            {
                assert(e.type.ty == Tvoid);
                result = null;
                return;
            }
            if (result.toBool().hasValue(false))
                res = false;
            else if (isTrueBool(result))
                res = true;
            else
            {
                error(e.loc, "`%s` does not evaluate to a `bool`", result.toChars());
                result = CTFEExp.cantexp;
                return;
            }
        }
        else
        {
            error(e.loc, "`%s` cannot be interpreted as a `bool`", result.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        incUsageCtfe(istate, e.e2.loc);

        if (goal != CTFEGoal.Nothing)
        {
            if (e.type.equals(Type.tbool))
                result = IntegerExp.createBool(res);
            else
            {
                emplaceExp!(IntegerExp)(pue, e.loc, res, e.type);
                result = pue.exp();
            }
        }
    }


    // Print a stack trace, starting from callingExp which called fd.
    // To shorten the stack trace, try to detect recursion.
    private void showCtfeBackTrace(CallExp callingExp, FuncDeclaration fd)
    {
        if (ctfeGlobals.stackTraceCallsToSuppress > 0)
        {
            --ctfeGlobals.stackTraceCallsToSuppress;
            return;
        }
        errorSupplemental(callingExp.loc, "called from here: `%s`", callingExp.toChars());
        // Quit if it's not worth trying to compress the stack trace
        if (ctfeGlobals.callDepth < 6 || global.params.v.verbose)
            return;
        // Recursion happens if the current function already exists in the call stack.
        int numToSuppress = 0;
        int recurseCount = 0;
        int depthSoFar = 0;
        InterState* lastRecurse = istate;
        for (InterState* cur = istate; cur; cur = cur.caller)
        {
            if (cur.fd == fd)
            {
                ++recurseCount;
                numToSuppress = depthSoFar;
                lastRecurse = cur;
            }
            ++depthSoFar;
        }
        // We need at least three calls to the same function, to make compression worthwhile
        if (recurseCount < 2)
            return;
        // We found a useful recursion.  Print all the calls involved in the recursion
        errorSupplemental(fd.loc, "%d recursive calls to function `%s`", recurseCount, fd.toChars());
        for (InterState* cur = istate; cur.fd != fd; cur = cur.caller)
        {
            errorSupplemental(cur.fd.loc, "recursively called from function `%s`", cur.fd.toChars());
        }
        // We probably didn't enter the recursion in this function.
        // Go deeper to find the real beginning.
        InterState* cur = istate;
        while (lastRecurse.caller && cur.fd == lastRecurse.caller.fd)
        {
            cur = cur.caller;
            lastRecurse = lastRecurse.caller;
            ++numToSuppress;
        }
        ctfeGlobals.stackTraceCallsToSuppress = numToSuppress;
    }

    override void visit(CallExp e)
    {
        debug (LOG)
        {
            printf("%s CallExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression pthis = null;
        FuncDeclaration fd = null;

        Expression ecall = interpretRegion(e.e1, istate);
        if (exceptionOrCant(ecall))
            return;

        if (auto dve = ecall.isDotVarExp())
        {
            // Calling a member function
            pthis = dve.e1;
            fd = dve.var.isFuncDeclaration();
            assert(fd);

            if (auto dte = pthis.isDotTypeExp())
                pthis = dte.e1;
        }
        else if (auto ve = ecall.isVarExp())
        {
            fd = ve.var.isFuncDeclaration();
            assert(fd);

            // If `_d_HookTraceImpl` is found, resolve the underlying hook and replace `e` and `fd` with it.
            removeHookTraceImpl(e, fd);

            if (fd.ident == Id.__ArrayPostblit || fd.ident == Id.__ArrayDtor)
            {
                assert(e.arguments.length == 1);
                Expression ea = (*e.arguments)[0];
                // printf("1 ea = %s %s\n", ea.type.toChars(), ea.toChars());
                if (auto se = ea.isSliceExp())
                    ea = se.e1;
                if (auto ce = ea.isCastExp())
                    ea = ce.e1;

                // printf("2 ea = %s, %s %s\n", ea.type.toChars(), EXPtoString(ea.op).ptr, ea.toChars());
                if (ea.op == EXP.variable || ea.op == EXP.symbolOffset)
                    result = getVarExp(e.loc, istate, (cast(SymbolExp)ea).var, CTFEGoal.RValue);
                else if (auto ae = ea.isAddrExp())
                    result = interpretRegion(ae.e1, istate);

                // https://issues.dlang.org/show_bug.cgi?id=18871
                // https://issues.dlang.org/show_bug.cgi?id=18819
                else if (auto ale = ea.isArrayLiteralExp())
                    result = interpretRegion(ale, istate);

                else
                    assert(0);
                if (CTFEExp.isCantExp(result))
                    return;

                if (fd.ident == Id.__ArrayPostblit)
                    result = evaluatePostblit(istate, result);
                else
                    result = evaluateDtor(istate, result);
                if (!result)
                    result = CTFEExp.voidexp;
                return;
            }
            else if (isArrayConstruction(fd.ident))
            {
                // In expressionsem.d, `T[x] ea = eb;` was lowered to:
                // `_d_array{,set}ctor(ea[], eb[]);`.
                // The following code will rewrite it back to `ea = eb` and
                // then interpret that expression.

                if (fd.ident == Id._d_arrayctor)
                    assert(e.arguments.length == 3);
                else
                    assert(e.arguments.length == 2);

                Expression ea = (*e.arguments)[0];
                if (ea.isCastExp)
                    ea = ea.isCastExp.e1;

                Expression eb = (*e.arguments)[1];
                if (eb.isCastExp() && fd.ident == Id._d_arrayctor)
                    eb = eb.isCastExp.e1;

                ConstructExp ce = new ConstructExp(e.loc, ea, eb);
                ce.type = ea.type;

                ce.type = ea.type;
                result = interpret(ce, istate);

                return;
            }
        }
        else if (auto soe = ecall.isSymOffExp())
        {
            fd = soe.var.isFuncDeclaration();
            assert(fd && soe.offset == 0);
        }
        else if (auto de = ecall.isDelegateExp())
        {
            // Calling a delegate
            fd = de.func;
            pthis = de.e1;

            // Special handling for: &nestedfunc --> DelegateExp(VarExp(nestedfunc), nestedfunc)
            if (auto ve = pthis.isVarExp())
                if (ve.var == fd)
                    pthis = null; // context is not necessary for CTFE
        }
        else if (auto fe = ecall.isFuncExp())
        {
            // Calling a delegate literal
            fd = fe.fd;
        }
        else
        {
            // delegate.funcptr()
            // others
            error(e.loc, "cannot call `%s` at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        if (!fd)
        {
            error(e.loc, "CTFE internal error: cannot evaluate `%s` at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        if (pthis)
        {
            // Member function call

            // Currently this is satisfied because closure is not yet supported.
            assert(!fd.isNested() || fd.needThis());

            if (pthis.op == EXP.typeid_)
            {
                error(pthis.loc, "static variable `%s` cannot be read at compile time", pthis.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            assert(pthis);

            if (pthis.op == EXP.null_)
            {
                assert(pthis.type.toBasetype().ty == Tclass);
                error(e.loc, "function call through null class reference `%s`", pthis.toChars());
                result = CTFEExp.cantexp;
                return;
            }

            assert(pthis.op == EXP.structLiteral || pthis.op == EXP.classReference || pthis.op == EXP.type);

            if (fd.isVirtual() && !e.directcall)
            {
                // Make a virtual function call.
                // Get the function from the vtable of the original class
                ClassDeclaration cd = pthis.isClassReferenceExp().originalClass();

                // We can't just use the vtable index to look it up, because
                // vtables for interfaces don't get populated until the glue layer.
                fd = cd.findFunc(fd.ident, fd.type.isTypeFunction());
                assert(fd);
            }
        }

        if (fd && fd.semanticRun >= PASS.semantic3done && fd.hasSemantic3Errors())
        {
            error(e.loc, "CTFE failed because of previous errors in `%s`", fd.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        // Check for built-in functions
        result = evaluateIfBuiltin(pue, istate, e.loc, fd, e.arguments, pthis);
        if (result)
            return;

        if (!fd.fbody)
        {
            error(e.loc, "`%s` cannot be interpreted at compile time, because it has no available source code", fd.toChars());
            result = CTFEExp.showcontext;
            return;
        }

        result = interpretFunction(pue, fd, istate, e.arguments, pthis);
        if (result.op == EXP.voidExpression)
            return;
        if (!exceptionOrCantInterpret(result))
        {
            if (goal != CTFEGoal.LValue) // Peel off CTFE reference if it's unnecessary
            {
                if (result == pue.exp())
                    result = pue.copy();
                result = interpret(pue, result, istate);
            }
        }
        if (!exceptionOrCantInterpret(result))
        {
            result = paintTypeOntoLiteral(pue, e.type, result);
            result.loc = e.loc;
        }
        else if (CTFEExp.isCantExp(result) && !global.gag)
            showCtfeBackTrace(e, fd); // Print a stack trace.
    }

    override void visit(CommaExp e)
    {
        /****************************************
         * Find the first non-comma expression.
         * Params:
         *      e = Expressions connected by commas
         * Returns:
         *      left-most non-comma expression
         */
        static inout(Expression) firstComma(inout Expression e)
        {
            Expression ex = cast()e;
            while (ex.op == EXP.comma)
                ex = (cast(CommaExp)ex).e1;
            return cast(inout)ex;

        }

        debug (LOG)
        {
            printf("%s CommaExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }

        bool isNewThrowableHook()
        {
            auto de = e.e1.isDeclarationExp();
            if (de is null)
                return false;

            auto vd = de.declaration.isVarDeclaration();
            if (vd is null)
                return false;

            auto ei = vd._init.isExpInitializer();
            if (ei is null)
                return false;

            auto ce = ei.exp.isConstructExp();
            if (ce is null)
                return false;

            return isRuntimeHook(ce.e2, Id._d_newThrowable) !is null;
        }

        if (auto ce = isRuntimeHook(e.e1, Id._d_arrayappendcTX))
        {
            // In expressionsem.d `arr ~= elem` was lowered to
            // `_d_arrayappendcTX(arr, elem), arr[arr.length - 1] = elem, elem;`.
            // The following code will rewrite it back to `arr ~= elem`
            // and then interpret that expression.
            assert(ce.arguments.length == 2);

            auto arr = (*ce.arguments)[0];
            auto elem = e.e2.isConstructExp().e2;
            assert(elem);

            auto cae = new CatAssignExp(e.loc, arr, elem);
            cae.type = arr.type;

            result = interpret(cae, istate);
            return;
        }
        else if (isNewThrowableHook())
        {
            // In expressionsem.d `throw new Exception(args)` was lowered to
            // `throw (tmp = _d_newThrowable!Exception(), tmp.ctor(args), tmp)`.
            // The following code will rewrite it back to `throw new Exception(args)`
            // and then interpret this expression instead.
            auto ce = e.e2.isCallExp();
            assert(ce);

            auto ne = new NewExp(e.loc, null, null, e.type, ce.arguments);
            ne.type = e.e1.type;

            result = interpret(ne, istate);
            return;
        }

        // If it creates a variable, and there's no context for
        // the variable to be created in, we need to create one now.
        InterState istateComma;
        if (!istate && firstComma(e.e1).op == EXP.declaration)
        {
            ctfeGlobals.stack.startFrame(null);
            istate = &istateComma;
        }

        void endTempStackFrame()
        {
            // If we created a temporary stack frame, end it now.
            if (istate == &istateComma)
                ctfeGlobals.stack.endFrame();
        }

        result = CTFEExp.cantexp;

        // If the comma returns a temporary variable, it needs to be an lvalue
        // (this is particularly important for struct constructors)
        if (e.e1.op == EXP.declaration &&
            e.e2.op == EXP.variable &&
            e.e1.isDeclarationExp().declaration == e.e2.isVarExp().var &&
            e.e2.isVarExp().var.storage_class & STC.ctfe)
        {
            VarExp ve = e.e2.isVarExp();
            VarDeclaration v = ve.var.isVarDeclaration();
            ctfeGlobals.stack.push(v);
            if (!v._init && !getValue(v))
            {
                setValue(v, copyLiteral(v.type.defaultInitLiteral(e.loc)).copy());
            }
            if (!getValue(v))
            {
                Expression newval = v._init.initializerToExpression();
                // Bug 4027. Copy constructors are a weird case where the
                // initializer is a void function (the variable is modified
                // through a reference parameter instead).
                newval = interpretRegion(newval, istate);
                if (exceptionOrCant(newval))
                    return endTempStackFrame();
                if (newval.op != EXP.voidExpression)
                {
                    // v isn't necessarily null.
                    setValueWithoutChecking(v, copyLiteral(newval).copy());
                }
            }
        }
        else
        {
            UnionExp ue = void;
            auto e1 = interpret(&ue, e.e1, istate, CTFEGoal.Nothing);
            if (exceptionOrCant(e1))
                return endTempStackFrame();
        }
        result = interpret(pue, e.e2, istate, goal);
        return endTempStackFrame();
    }

    override void visit(CondExp e)
    {
        debug (LOG)
        {
            printf("%s CondExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        UnionExp uecond = void;
        Expression econd;
        econd = interpret(&uecond, e.econd, istate);
        if (exceptionOrCant(econd))
            return;

        if (isPointer(e.econd.type))
        {
            if (econd.op != EXP.null_)
            {
                econd = IntegerExp.createBool(true);
            }
        }

        if (isTrueBool(econd))
        {
            result = interpret(pue, e.e1, istate, goal);
            incUsageCtfe(istate, e.e1.loc);
        }
        else if (econd.toBool().hasValue(false))
        {
            result = interpret(pue, e.e2, istate, goal);
            incUsageCtfe(istate, e.e2.loc);
        }
        else
        {
            error(e.loc, "`%s` does not evaluate to boolean result at compile time", e.econd.toChars());
            result = CTFEExp.cantexp;
        }
    }

    override void visit(ArrayLengthExp e)
    {
        debug (LOG)
        {
            printf("%s ArrayLengthExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        UnionExp ue1;
        Expression e1 = interpret(&ue1, e.e1, istate);
        assert(e1);
        if (exceptionOrCant(e1))
            return;
        if (e1.op != EXP.string_ && e1.op != EXP.arrayLiteral && e1.op != EXP.slice && e1.op != EXP.null_)
        {
            error(e.loc, "`%s` cannot be evaluated at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        emplaceExp!(IntegerExp)(pue, e.loc, resolveArrayLength(e1), e.type);
        result = pue.exp();
    }

    /**
     * Interpret the vector expression as an array literal.
     * Params:
     *    pue = non-null pointer to temporary storage that can be used to store the return value
     *    e = Expression to interpret
     * Returns:
     *    resulting array literal or 'e' if unable to interpret
     */
    static Expression interpretVectorToArray(UnionExp* pue, VectorExp e)
    {
        if (auto ale = e.e1.isArrayLiteralExp())
            return ale;         // it's already an array literal
        if (e.e1.op == EXP.int64 || e.e1.op == EXP.float64)
        {
            // Convert literal __vector(int) -> __vector([array])
            auto elements = new Expressions(e.dim);
            foreach (ref element; *elements)
                element = copyLiteral(e.e1).copy();
            auto type = (e.type.ty == Tvector) ? e.type.isTypeVector().basetype : e.type.isTypeSArray();
            assert(type);
            emplaceExp!(ArrayLiteralExp)(pue, e.loc, type, elements);
            auto ale = pue.exp().isArrayLiteralExp();
            ale.ownedByCtfe = OwnedBy.ctfe;
            return ale;
        }
        return e;
    }

    override void visit(VectorExp e)
    {
        debug (LOG)
        {
            printf("%s VectorExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.ownedByCtfe >= OwnedBy.ctfe) // We've already interpreted all the elements
        {
            result = e;
            return;
        }
        Expression e1 = interpret(pue, e.e1, istate);
        assert(e1);
        if (exceptionOrCant(e1))
            return;
        if (e1.op != EXP.arrayLiteral && e1.op != EXP.int64 && e1.op != EXP.float64)
        {
            error(e.loc, "`%s` cannot be evaluated at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        if (e1 == pue.exp())
            e1 = pue.copy();
        emplaceExp!(VectorExp)(pue, e.loc, e1, e.to);
        auto ve = pue.exp().isVectorExp();
        ve.type = e.type;
        ve.dim = e.dim;
        ve.ownedByCtfe = OwnedBy.ctfe;
        result = ve;
    }

    override void visit(VectorArrayExp e)
    {
        debug (LOG)
        {
            printf("%s VectorArrayExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression e1 = interpret(pue, e.e1, istate);
        assert(e1);
        if (exceptionOrCant(e1))
            return;
        if (auto ve = e1.isVectorExp())
        {
            result = interpretVectorToArray(pue, ve);
            if (result.op != EXP.vector)
                return;
        }
        error(e.loc, "`%s` cannot be evaluated at compile time", e.toChars());
        result = CTFEExp.cantexp;
    }

    override void visit(DelegatePtrExp e)
    {
        debug (LOG)
        {
            printf("%s DelegatePtrExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression e1 = interpret(pue, e.e1, istate);
        assert(e1);
        if (exceptionOrCant(e1))
            return;
        error(e.loc, "`%s` cannot be evaluated at compile time", e.toChars());
        result = CTFEExp.cantexp;
    }

    override void visit(DelegateFuncptrExp e)
    {
        debug (LOG)
        {
            printf("%s DelegateFuncptrExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression e1 = interpret(pue, e.e1, istate);
        assert(e1);
        if (exceptionOrCant(e1))
            return;
        error(e.loc, "`%s` cannot be evaluated at compile time", e.toChars());
        result = CTFEExp.cantexp;
    }

    static bool resolveIndexing(IndexExp e, InterState* istate, Expression* pagg, uinteger_t* pidx, bool modify)
    {
        assert(e.e1.type.toBasetype().ty != Taarray);

        if (e.e1.type.toBasetype().ty == Tpointer)
        {
            // Indexing a pointer. Note that there is no $ in this case.
            Expression e1 = interpretRegion(e.e1, istate);
            if (exceptionOrCantInterpret(e1))
                return false;

            Expression e2 = interpretRegion(e.e2, istate);
            if (exceptionOrCantInterpret(e2))
                return false;
            sinteger_t indx = e2.toInteger();

            dinteger_t ofs;
            Expression agg = getAggregateFromPointer(e1, &ofs);

            if (agg.op == EXP.null_)
            {
                error(e.loc, "cannot index through null pointer `%s`", e.e1.toChars());
                return false;
            }
            if (agg.op == EXP.int64)
            {
                error(e.loc, "cannot index through invalid pointer `%s` of value `%s`", e.e1.toChars(), e1.toChars());
                return false;
            }
            // Pointer to a non-array variable
            if (agg.op == EXP.symbolOffset)
            {
                error(e.loc, "mutable variable `%s` cannot be %s at compile time, even through a pointer", cast(char*)(modify ? "modified" : "read"), agg.isSymOffExp().var.toChars());
                return false;
            }

            if (agg.op == EXP.arrayLiteral || agg.op == EXP.string_)
            {
                dinteger_t len = resolveArrayLength(agg);
                if (ofs + indx >= len)
                {
                    error(e.loc, "pointer index `[%lld]` exceeds allocated memory block `[0..%lld]`", ofs + indx, len);
                    return false;
                }
            }
            else
            {
                if (ofs + indx != 0)
                {
                    error(e.loc, "pointer index `[%lld]` lies outside memory block `[0..1]`", ofs + indx);
                    return false;
                }
            }
            *pagg = agg;
            *pidx = ofs + indx;
            return true;
        }

        Expression e1 = interpretRegion(e.e1, istate);
        if (exceptionOrCantInterpret(e1))
            return false;
        if (e1.op == EXP.null_)
        {
            error(e.loc, "cannot index null array `%s`", e.e1.toChars());
            return false;
        }
        if (auto ve = e1.isVectorExp())
        {
            UnionExp ue = void;
            e1 = interpretVectorToArray(&ue, ve);
            e1 = (e1 == ue.exp()) ? ue.copy() : e1;
        }

        // Set the $ variable, and find the array literal to modify
        dinteger_t len;
        if (e1.op == EXP.variable && e1.type.toBasetype().ty == Tsarray)
            len = e1.type.toBasetype().isTypeSArray().dim.toInteger();
        else
        {
            if (e1.op != EXP.arrayLiteral && e1.op != EXP.string_ && e1.op != EXP.slice && e1.op != EXP.vector)
            {
                error(e.loc, "cannot determine length of `%s` at compile time", e.e1.toChars());
                return false;
            }
            len = resolveArrayLength(e1);
        }

        if (e.lengthVar)
        {
            Expression dollarExp = ctfeEmplaceExp!IntegerExp(e.loc, len, Type.tsize_t);
            ctfeGlobals.stack.push(e.lengthVar);
            setValue(e.lengthVar, dollarExp);
        }
        Expression e2 = interpretRegion(e.e2, istate);
        if (e.lengthVar)
            ctfeGlobals.stack.pop(e.lengthVar); // $ is defined only inside []
        if (exceptionOrCantInterpret(e2))
            return false;
        if (e2.op != EXP.int64)
        {
            error(e.loc, "CTFE internal error: non-integral index `[%s]`", e.e2.toChars());
            return false;
        }

        if (auto se = e1.isSliceExp())
        {
            // Simplify index of slice: agg[lwr..upr][indx] --> agg[indx']
            uinteger_t index = e2.toInteger();
            uinteger_t ilwr = se.lwr.toInteger();
            uinteger_t iupr = se.upr.toInteger();

            if (index > iupr - ilwr)
            {
                error(e.loc, "index %llu exceeds array length %llu", index, iupr - ilwr);
                return false;
            }
            *pagg = e1.isSliceExp().e1;
            *pidx = index + ilwr;
        }
        else
        {
            *pagg = e1;
            *pidx = e2.toInteger();
            if (len <= *pidx)
            {
                error(e.loc, "array index %lld is out of bounds `[0..%lld]`", *pidx, len);
                return false;
            }
        }
        return true;
    }

    override void visit(IndexExp e)
    {
        debug (LOG)
        {
            printf("%s IndexExp::interpret() %s, goal = %d\n", e.loc.toChars(), e.toChars(), goal);
        }
        if (e.e1.type.toBasetype().ty == Tpointer)
        {
            Expression agg;
            uinteger_t indexToAccess;
            if (!resolveIndexing(e, istate, &agg, &indexToAccess, false))
            {
                result = CTFEExp.cantexp;
                return;
            }
            if (agg.op == EXP.arrayLiteral || agg.op == EXP.string_)
            {
                if (goal == CTFEGoal.LValue)
                {
                    // if we need a reference, IndexExp shouldn't be interpreting
                    // the expression to a value, it should stay as a reference
                    emplaceExp!(IndexExp)(pue, e.loc, agg, ctfeEmplaceExp!IntegerExp(e.e2.loc, indexToAccess, e.e2.type));
                    result = pue.exp();
                    result.type = e.type;
                    return;
                }
                result = ctfeIndex(pue, e.loc, e.type, agg, indexToAccess);
                return;
            }
            else
            {
                assert(indexToAccess == 0);
                result = interpretRegion(agg, istate, goal);
                if (exceptionOrCant(result))
                    return;
                result = paintTypeOntoLiteral(pue, e.type, result);
                return;
            }
        }

        if (e.e1.type.toBasetype().ty == Taarray)
        {
            Expression e1 = interpretRegion(e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            if (e1.op == EXP.null_)
            {
                if (goal == CTFEGoal.LValue && e1.type.ty == Taarray && e.modifiable)
                {
                    assert(0); // does not reach here?
                }
                error(e.loc, "cannot index null array `%s`", e.e1.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            Expression e2 = interpretRegion(e.e2, istate);
            if (exceptionOrCant(e2))
                return;

            if (goal == CTFEGoal.LValue)
            {
                // Pointer or reference of a scalar type
                if (e1 == e.e1 && e2 == e.e2)
                    result = e;
                else
                {
                    emplaceExp!(IndexExp)(pue, e.loc, e1, e2);
                    result = pue.exp();
                    result.type = e.type;
                }
                return;
            }

            assert(e1.op == EXP.assocArrayLiteral);
            UnionExp e2tmp = void;
            e2 = resolveSlice(e2, &e2tmp);
            result = findKeyInAA(e.loc, e1.isAssocArrayLiteralExp(), e2);
            if (!result)
            {
                error(e.loc, "key `%s` not found in associative array `%s`", e2.toChars(), e.e1.toChars());
                result = CTFEExp.cantexp;
            }
            return;
        }

        Expression agg;
        uinteger_t indexToAccess;
        if (!resolveIndexing(e, istate, &agg, &indexToAccess, false))
        {
            result = CTFEExp.cantexp;
            return;
        }

        if (goal == CTFEGoal.LValue)
        {
            Expression e2 = ctfeEmplaceExp!IntegerExp(e.e2.loc, indexToAccess, Type.tsize_t);
            emplaceExp!(IndexExp)(pue, e.loc, agg, e2);
            result = pue.exp();
            result.type = e.type;
            return;
        }

        result = ctfeIndex(pue, e.loc, e.type, agg, indexToAccess);
        if (exceptionOrCant(result))
            return;
        if (result.op == EXP.void_)
        {
            error(e.loc, "`%s` is used before initialized", e.toChars());
            errorSupplemental(result.loc, "originally uninitialized here");
            result = CTFEExp.cantexp;
            return;
        }
        if (result == pue.exp())
            result = result.copy();
    }

    override void visit(SliceExp e)
    {
        debug (LOG)
        {
            printf("%s SliceExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        if (e.e1.type.toBasetype().ty == Tpointer)
        {
            // Slicing a pointer. Note that there is no $ in this case.
            Expression e1 = interpretRegion(e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            if (e1.op == EXP.int64)
            {
                error(e.loc, "cannot slice invalid pointer `%s` of value `%s`", e.e1.toChars(), e1.toChars());
                result = CTFEExp.cantexp;
                return;
            }

            /* Evaluate lower and upper bounds of slice
             */
            Expression lwr = interpretRegion(e.lwr, istate);
            if (exceptionOrCant(lwr))
                return;
            Expression upr = interpretRegion(e.upr, istate);
            if (exceptionOrCant(upr))
                return;
            uinteger_t ilwr = lwr.toInteger();
            uinteger_t iupr = upr.toInteger();

            dinteger_t ofs;
            Expression agg = getAggregateFromPointer(e1, &ofs);
            ilwr += ofs;
            iupr += ofs;
            if (agg.op == EXP.null_)
            {
                if (iupr == ilwr)
                {
                    result = ctfeEmplaceExp!NullExp(e.loc);
                    result.type = e.type;
                    return;
                }
                error(e.loc, "cannot slice null pointer `%s`", e.e1.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            if (agg.op == EXP.symbolOffset)
            {
                error(e.loc, "slicing pointers to static variables is not supported in CTFE");
                result = CTFEExp.cantexp;
                return;
            }
            if (agg.op != EXP.arrayLiteral && agg.op != EXP.string_)
            {
                error(e.loc, "pointer `%s` cannot be sliced at compile time (it does not point to an array)", e.e1.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            assert(agg.op == EXP.arrayLiteral || agg.op == EXP.string_);
            dinteger_t len = ArrayLength(Type.tsize_t, agg).exp().toInteger();
            //Type *pointee = ((TypePointer *)agg.type)->next;
            if (sliceBoundsCheck(0, len, ilwr, iupr))
            {
                error(e.loc, "pointer slice `[%lld..%lld]` exceeds allocated memory block `[0..%lld]`", ilwr, iupr, len);
                result = CTFEExp.cantexp;
                return;
            }
            if (ofs != 0)
            {
                lwr = ctfeEmplaceExp!IntegerExp(e.loc, ilwr, lwr.type);
                upr = ctfeEmplaceExp!IntegerExp(e.loc, iupr, upr.type);
            }
            emplaceExp!(SliceExp)(pue, e.loc, agg, lwr, upr);
            result = pue.exp();
            result.type = e.type;
            return;
        }

        CTFEGoal goal1 = CTFEGoal.RValue;
        if (goal == CTFEGoal.LValue)
        {
            if (e.e1.type.toBasetype().ty == Tsarray)
                if (auto ve = e.e1.isVarExp())
                    if (auto vd = ve.var.isVarDeclaration())
                        if (vd.storage_class & STC.ref_)
                            goal1 = CTFEGoal.LValue;
        }
        Expression e1 = interpret(e.e1, istate, goal1);
        if (exceptionOrCant(e1))
            return;

        if (!e.lwr)
        {
            result = paintTypeOntoLiteral(pue, e.type, e1);
            return;
        }
        if (auto ve = e1.isVectorExp())
        {
            e1 = interpretVectorToArray(pue, ve);
            e1 = (e1 == pue.exp()) ? pue.copy() : e1;
        }

        /* Set dollar to the length of the array
         */
        uinteger_t dollar;
        if ((e1.op == EXP.variable || e1.op == EXP.dotVariable) && e1.type.toBasetype().ty == Tsarray)
            dollar = e1.type.toBasetype().isTypeSArray().dim.toInteger();
        else
        {
            if (e1.op != EXP.arrayLiteral && e1.op != EXP.string_ && e1.op != EXP.null_ && e1.op != EXP.slice && e1.op != EXP.vector)
            {
                error(e.loc, "cannot determine length of `%s` at compile time", e1.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            dollar = resolveArrayLength(e1);
        }

        /* Set the $ variable
         */
        if (e.lengthVar)
        {
            auto dollarExp = ctfeEmplaceExp!IntegerExp(e.loc, dollar, Type.tsize_t);
            ctfeGlobals.stack.push(e.lengthVar);
            setValue(e.lengthVar, dollarExp);
        }

        /* Evaluate lower and upper bounds of slice
         */
        Expression lwr = interpretRegion(e.lwr, istate);
        if (exceptionOrCant(lwr))
        {
            if (e.lengthVar)
                ctfeGlobals.stack.pop(e.lengthVar);
            return;
        }
        Expression upr = interpretRegion(e.upr, istate);
        if (exceptionOrCant(upr))
        {
            if (e.lengthVar)
                ctfeGlobals.stack.pop(e.lengthVar);
            return;
        }
        if (e.lengthVar)
            ctfeGlobals.stack.pop(e.lengthVar); // $ is defined only inside [L..U]

        uinteger_t ilwr = lwr.toInteger();
        uinteger_t iupr = upr.toInteger();
        if (e1.op == EXP.null_)
        {
            if (ilwr == 0 && iupr == 0)
            {
                result = e1;
                return;
            }
            error(e1.loc, "slice `[%llu..%llu]` is out of bounds", ilwr, iupr);
            result = CTFEExp.cantexp;
            return;
        }
        if (auto se = e1.isSliceExp())
        {
            // Simplify slice of slice:
            //  aggregate[lo1..up1][lwr..upr] ---> aggregate[lwr'..upr']
            uinteger_t lo1 = se.lwr.toInteger();
            uinteger_t up1 = se.upr.toInteger();
            if (sliceBoundsCheck(0, up1 - lo1, ilwr, iupr))
            {
                error(e.loc, "slice `[%llu..%llu]` exceeds array bounds `[0..%llu]`", ilwr, iupr, up1 - lo1);
                result = CTFEExp.cantexp;
                return;
            }
            ilwr += lo1;
            iupr += lo1;
            emplaceExp!(SliceExp)(pue, e.loc, se.e1,
                ctfeEmplaceExp!IntegerExp(e.loc, ilwr, lwr.type),
                ctfeEmplaceExp!IntegerExp(e.loc, iupr, upr.type));
            result = pue.exp();
            result.type = e.type;
            return;
        }
        if (e1.op == EXP.arrayLiteral || e1.op == EXP.string_)
        {
            if (sliceBoundsCheck(0, dollar, ilwr, iupr))
            {
                error(e.loc, "slice `[%lld..%lld]` exceeds array bounds `[0..%lld]`", ilwr, iupr, dollar);
                result = CTFEExp.cantexp;
                return;
            }
        }
        emplaceExp!(SliceExp)(pue, e.loc, e1, lwr, upr);
        result = pue.exp();
        result.type = e.type;
    }

    override void visit(InExp e)
    {
        debug (LOG)
        {
            printf("%s InExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression e1 = interpretRegion(e.e1, istate);
        if (exceptionOrCant(e1))
            return;
        Expression e2 = interpretRegion(e.e2, istate);
        if (exceptionOrCant(e2))
            return;
        if (e2.op == EXP.null_)
        {
            emplaceExp!(NullExp)(pue, e.loc, e.type);
            result = pue.exp();
            return;
        }
        if (e2.op != EXP.assocArrayLiteral)
        {
            error(e.loc, "`%s` cannot be interpreted at compile time", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        e1 = resolveSlice(e1);
        result = findKeyInAA(e.loc, e2.isAssocArrayLiteralExp(), e1);
        if (exceptionOrCant(result))
            return;
        if (!result)
        {
            emplaceExp!(NullExp)(pue, e.loc, e.type);
            result = pue.exp();
        }
        else
        {
            // Create a CTFE pointer &aa[index]
            result = ctfeEmplaceExp!IndexExp(e.loc, e2, e1);
            result.type = e.type.nextOf();
            emplaceExp!(AddrExp)(pue, e.loc, result, e.type);
            result = pue.exp();
        }
    }

    override void visit(CatExp e)
    {
        debug (LOG)
        {
            printf("%s CatExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }

        UnionExp ue1 = void;
        Expression e1 = interpret(&ue1, e.e1, istate);
        if (exceptionOrCant(e1))
            return;

        UnionExp ue2 = void;
        Expression e2 = interpret(&ue2, e.e2, istate);
        if (exceptionOrCant(e2))
            return;

        UnionExp e1tmp = void;
        e1 = resolveSlice(e1, &e1tmp);

        UnionExp e2tmp = void;
        e2 = resolveSlice(e2, &e2tmp);

        /* e1 and e2 can't go on the stack because of x~[y] and [x]~y will
         * result in [x,y] and then x or y is on the stack.
         * But if they are both strings, we can, because it isn't the x~[y] case.
         */
        if (!(e1.op == EXP.string_ && e2.op == EXP.string_))
        {
            if (e1 == ue1.exp())
                e1 = ue1.copy();
            if (e2 == ue2.exp())
                e2 = ue2.copy();
        }

        Expression prepareCatOperand(Expression exp)
        {
            /* Convert `elem ~ array` to `[elem] ~ array` if `elem` is itself an
             * array. This is needed because interpreting the `CatExp` calls
             * `Cat()`, which cannot handle concatenations between different
             * types, except for strings and chars.
             */
            auto tb = e.type.toBasetype();
            auto tbNext = tb.nextOf();
            auto expTb = exp.type.toBasetype();

            if (exp.type.implicitConvTo(tbNext) >= MATCH.convert &&
                tb.isStaticOrDynamicArray() && expTb.isStaticOrDynamicArray())
                return new ArrayLiteralExp(exp.loc, e.type, exp);
            return exp;
        }

        *pue = ctfeCat(e.loc, e.type, prepareCatOperand(e1), prepareCatOperand(e2));
        result = pue.exp();

        if (CTFEExp.isCantExp(result))
        {
            error(e.loc, "`%s` cannot be interpreted at compile time", e.toChars());
            return;
        }
        // We know we still own it, because we interpreted both e1 and e2
        if (auto ale = result.isArrayLiteralExp())
        {
            ale.ownedByCtfe = OwnedBy.ctfe;

            // https://issues.dlang.org/show_bug.cgi?id=14686
            foreach (elem; *ale.elements)
            {
                Expression ex = evaluatePostblit(istate, elem);
                if (exceptionOrCant(ex))
                    return;
            }
        }
        else if (auto se = result.isStringExp())
            se.ownedByCtfe = OwnedBy.ctfe;
    }

    override void visit(DeleteExp e)
    {
        debug (LOG)
        {
            printf("%s DeleteExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        result = interpretRegion(e.e1, istate);
        if (exceptionOrCant(result))
            return;

        if (result.op == EXP.null_)
        {
            result = CTFEExp.voidexp;
            return;
        }

        auto tb = e.e1.type.toBasetype();
        switch (tb.ty)
        {
        case Tclass:
            if (result.op != EXP.classReference)
            {
                error(e.loc, "`delete` on invalid class reference `%s`", result.toChars());
                result = CTFEExp.cantexp;
                return;
            }

            auto cre = result.isClassReferenceExp();
            auto cd = cre.originalClass();

            // Find dtor(s) in inheritance chain
            do
            {
                if (cd.dtor)
                {
                    result = interpretFunction(pue, cd.dtor, istate, null, cre);
                    if (exceptionOrCant(result))
                        return;

                    // Dtors of Non-extern(D) classes use implicit chaining (like structs)
                    import dmd.aggregate : ClassKind;
                    if (cd.classKind != ClassKind.d)
                        break;
                }

                // Emulate manual chaining as done in rt_finalize2
                cd = cd.baseClass;

            } while (cd); // Stop after Object

            break;

        default:
            assert(0);
        }
        result = CTFEExp.voidexp;
    }

    override void visit(CastExp e)
    {
        debug (LOG)
        {
            printf("%s CastExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression e1 = interpretRegion(e.e1, istate, goal);
        if (exceptionOrCant(e1))
            return;
        // If the expression has been cast to void, do nothing.
        if (e.to.ty == Tvoid)
        {
            result = CTFEExp.voidexp;
            return;
        }
        if (e.to.ty == Tpointer && e1.op != EXP.null_)
        {
            Type pointee = (cast(TypePointer)e.type).next;
            // Implement special cases of normally-unsafe casts
            if (e1.op == EXP.int64)
            {
                // Happens with Windows HANDLEs, for example.
                result = paintTypeOntoLiteral(pue, e.to, e1);
                return;
            }

            bool castToSarrayPointer = false;
            bool castBackFromVoid = false;
            if (e1.type.isStaticOrDynamicArray() || e1.type.ty == Tpointer)
            {
                // Check for unsupported type painting operations
                // For slices, we need the type being sliced,
                // since it may have already been type painted
                Type elemtype = e1.type.nextOf();
                if (auto se = e1.isSliceExp())
                    elemtype = se.e1.type.nextOf();

                // Allow casts from X* to void *, and X** to void** for any X.
                // But don't allow cast from X* to void**.
                // So, we strip all matching * from source and target to find X.
                // Allow casts to X* from void* only if the 'void' was originally an X;
                // we check this later on.
                Type ultimatePointee = pointee;
                Type ultimateSrc = elemtype;
                while (ultimatePointee.ty == Tpointer && ultimateSrc.ty == Tpointer)
                {
                    ultimatePointee = ultimatePointee.nextOf();
                    ultimateSrc = ultimateSrc.nextOf();
                }
                if (ultimatePointee.ty == Tsarray && ultimatePointee.nextOf().equivalent(ultimateSrc))
                {
                    castToSarrayPointer = true;
                }
                else if (ultimatePointee.ty != Tvoid && ultimateSrc.ty != Tvoid && !isSafePointerCast(elemtype, pointee))
                {
                    error(e.loc, "reinterpreting cast from `%s*` to `%s*` is not supported in CTFE", elemtype.toChars(), pointee.toChars());
                    result = CTFEExp.cantexp;
                    return;
                }
                if (ultimateSrc.ty == Tvoid)
                    castBackFromVoid = true;
            }

            if (auto se = e1.isSliceExp())
            {
                if (se.e1.op == EXP.null_)
                {
                    result = paintTypeOntoLiteral(pue, e.type, se.e1);
                    return;
                }
                // Create a CTFE pointer &aggregate[1..2]
                auto ei = ctfeEmplaceExp!IndexExp(e.loc, se.e1, se.lwr);
                ei.type = e.type.nextOf();
                emplaceExp!(AddrExp)(pue, e.loc, ei, e.type);
                result = pue.exp();
                return;
            }
            if (e1.op == EXP.arrayLiteral || e1.op == EXP.string_)
            {
                // Create a CTFE pointer &[1,2,3][0] or &"abc"[0]
                auto ei = ctfeEmplaceExp!IndexExp(e.loc, e1, ctfeEmplaceExp!IntegerExp(e.loc, 0, Type.tsize_t));
                ei.type = e.type.nextOf();
                emplaceExp!(AddrExp)(pue, e.loc, ei, e.type);
                result = pue.exp();
                return;
            }
            if (e1.op == EXP.index && !e1.isIndexExp().e1.type.equals(e1.type))
            {
                // type painting operation
                IndexExp ie = e1.isIndexExp();
                if (castBackFromVoid)
                {
                    // get the original type. For strings, it's just the type...
                    Type origType = ie.e1.type.nextOf();
                    // ..but for arrays of type void*, it's the type of the element
                    if (ie.e1.op == EXP.arrayLiteral && ie.e2.op == EXP.int64)
                    {
                        ArrayLiteralExp ale = ie.e1.isArrayLiteralExp();
                        const indx = cast(size_t)ie.e2.toInteger();
                        if (indx < ale.elements.length)
                        {
                            if (Expression xx = (*ale.elements)[indx])
                            {
                                if (auto iex = xx.isIndexExp())
                                    origType = iex.e1.type.nextOf();
                                else if (auto ae = xx.isAddrExp())
                                    origType = ae.e1.type;
                                else if (auto ve = xx.isVarExp())
                                    origType = ve.var.type;
                            }
                        }
                    }
                    if (!isSafePointerCast(origType, pointee))
                    {
                        error(e.loc, "using `void*` to reinterpret cast from `%s*` to `%s*` is not supported in CTFE", origType.toChars(), pointee.toChars());
                        result = CTFEExp.cantexp;
                        return;
                    }
                }
                emplaceExp!(IndexExp)(pue, e1.loc, ie.e1, ie.e2);
                result = pue.exp();
                result.type = e.type;
                return;
            }

            if (auto ae = e1.isAddrExp())
            {
                Type origType = ae.e1.type;
                if (isSafePointerCast(origType, pointee))
                {
                    emplaceExp!(AddrExp)(pue, e.loc, ae.e1, e.type);
                    result = pue.exp();
                    return;
                }

                if (castToSarrayPointer && pointee.toBasetype().ty == Tsarray && ae.e1.op == EXP.index)
                {
                    // &val[idx]
                    dinteger_t dim = (cast(TypeSArray)pointee.toBasetype()).dim.toInteger();
                    IndexExp ie = ae.e1.isIndexExp();
                    Expression lwr = ie.e2;
                    Expression upr = ctfeEmplaceExp!IntegerExp(ie.e2.loc, ie.e2.toInteger() + dim, Type.tsize_t);

                    // Create a CTFE pointer &val[idx..idx+dim]
                    auto er = ctfeEmplaceExp!SliceExp(e.loc, ie.e1, lwr, upr);
                    er.type = pointee;
                    emplaceExp!(AddrExp)(pue, e.loc, er, e.type);
                    result = pue.exp();
                    return;
                }
            }

            if (e1.op == EXP.variable || e1.op == EXP.symbolOffset)
            {
                // type painting operation
                Type origType = (cast(SymbolExp)e1).var.type;
                if (castBackFromVoid && !isSafePointerCast(origType, pointee))
                {
                    error(e.loc, "using `void*` to reinterpret cast from `%s*` to `%s*` is not supported in CTFE", origType.toChars(), pointee.toChars());
                    result = CTFEExp.cantexp;
                    return;
                }
                if (auto ve = e1.isVarExp())
                    emplaceExp!(VarExp)(pue, e.loc, ve.var);
                else
                    emplaceExp!(SymOffExp)(pue, e.loc, e1.isSymOffExp().var, e1.isSymOffExp().offset);
                result = pue.exp();
                result.type = e.to;
                return;
            }

            // Check if we have a null pointer (eg, inside a struct)
            e1 = interpretRegion(e1, istate);
            if (e1.op != EXP.null_)
            {
                error(e.loc, "pointer cast from `%s` to `%s` is not supported at compile time", e1.type.toChars(), e.to.toChars());
                result = CTFEExp.cantexp;
                return;
            }
        }
        if (e.to.ty == Tsarray && e.e1.type.ty == Tvector)
        {
            // Special handling for: cast(float[4])__vector([w, x, y, z])
            e1 = interpretRegion(e.e1, istate);
            if (exceptionOrCant(e1))
                return;
            assert(e1.op == EXP.vector);
            e1 = interpretVectorToArray(pue, e1.isVectorExp());
        }
        if (e.to.ty == Tarray && e1.op == EXP.slice)
        {
            // Note that the slice may be void[], so when checking for dangerous
            // casts, we need to use the original type, which is se.e1.
            SliceExp se = e1.isSliceExp();
            if (!isSafePointerCast(se.e1.type.nextOf(), e.to.nextOf()))
            {
                error(e.loc, "array cast from `%s` to `%s` is not supported at compile time", se.e1.type.toChars(), e.to.toChars());
                result = CTFEExp.cantexp;
                return;
            }
            emplaceExp!(SliceExp)(pue, e1.loc, se.e1, se.lwr, se.upr);
            result = pue.exp();
            result.type = e.to;
            return;
        }

        // Disallow array type painting, except for conversions between built-in
        // types of identical size.
        if (e.to.isStaticOrDynamicArray() && e1.type.isStaticOrDynamicArray() && !isSafePointerCast(e1.type.nextOf(), e.to.nextOf()))
        {
            auto se = e1.isStringExp();
            // Allow casting a hex string literal to short[], int[] or long[]
            if (se && se.hexString && se.postfix == StringExp.NoPostfix && e.to.nextOf().isIntegral)
            {
                const sz = cast(size_t) e.to.nextOf().size;
                if ((se.len % sz) != 0)
                {
                    error(e.loc, "hex string length %d must be a multiple of %d to cast to `%s`",
                        cast(int) se.len, cast(int) sz, e.to.toChars());
                    result = CTFEExp.cantexp;
                    return;
                }

                auto str = arrayCastBigEndian(se.peekData(), sz);
                emplaceExp!(StringExp)(pue, e1.loc, str, se.len / sz, cast(ubyte) sz);
                result = pue.exp();
                result.type = e.to;
                return;
            }
            error(e.loc, "array cast from `%s` to `%s` is not supported at compile time", e1.type.toChars(), e.to.toChars());
            if (se && se.hexString && se.postfix != StringExp.NoPostfix)
                errorSupplemental(e.loc, "perhaps remove postfix `%.*s` from hex string", 1, &se.postfix);

            result = CTFEExp.cantexp;
            return;
        }
        if (e.to.ty == Tsarray)
            e1 = resolveSlice(e1);

        auto tobt = e.to.toBasetype();
        if (tobt.ty == Tbool && e1.type.ty == Tpointer)
        {
            emplaceExp!(IntegerExp)(pue, e.loc, e1.op != EXP.null_, e.to);
            result = pue.exp();
            return;
        }
        else if (tobt.isTypeBasic() && e1.op == EXP.null_)
        {
            if (tobt.isIntegral())
                emplaceExp!(IntegerExp)(pue, e.loc, 0, e.to);
            else if (tobt.isReal())
                emplaceExp!(RealExp)(pue, e.loc, CTFloat.zero, e.to);
            result = pue.exp();
            return;
        }
        result = ctfeCast(pue, e.loc, e.type, e.to, e1, true);
    }

    override void visit(AssertExp e)
    {
        debug (LOG)
        {
            printf("%s AssertExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression e1 = interpret(pue, e.e1, istate);
        if (exceptionOrCant(e1))
            return;
        if (isTrueBool(e1))
        {
        }
        else if (e1.toBool().hasValue(false))
        {
            if (e.msg)
            {
                UnionExp ue = void;
                result = interpret(&ue, e.msg, istate);
                if (exceptionOrCant(result))
                    return;
                result = scrubReturnValue(e.loc, result);
                if (StringExp se = result.toStringExp())
                    error(e.loc, "%s", se.toStringz().ptr);
                else
                    error(e.loc, "%s", result.toChars());
            }
            else
                error(e.loc, "`%s` failed", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        else
        {
            error(e.loc, "`%s` is not a compile time boolean expression", e1.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        result = e1;
        return;
    }

    override void visit(ThrowExp te)
    {
        debug (LOG)
        {
            printf("%s ThrowExpression::interpret()\n", te.loc.toChars());
        }
        interpretThrow(result, te.e1, te.loc, istate);
    }

    override void visit(PtrExp e)
    {
        // Called for both lvalues and rvalues
        const lvalue = goal == CTFEGoal.LValue;
        debug (LOG)
        {
            printf("%s PtrExp::interpret(%d) %s, %s\n", e.loc.toChars(), lvalue, e.type.toChars(), e.toChars());
        }

        // Check for int<->float and long<->double casts.
        if (auto soe1 = e.e1.isSymOffExp())
            if (soe1.offset == 0 && soe1.var.isVarDeclaration() && isFloatIntPaint(e.type, soe1.var.type))
            {
                // *(cast(int*)&v), where v is a float variable
                result = paintFloatInt(pue, getVarExp(e.loc, istate, soe1.var, CTFEGoal.RValue), e.type);
                return;
            }

        if (auto ce1 = e.e1.isCastExp())
            if (auto ae11 = ce1.e1.isAddrExp())
            {
                // *(cast(int*)&x), where x is a float expression
                Expression x = ae11.e1;
                if (isFloatIntPaint(e.type, x.type))
                {
                    result = paintFloatInt(pue, interpretRegion(x, istate), e.type);
                    return;
                }
            }

        // Constant fold *(&structliteral + offset)
        if (auto ae = e.e1.isAddExp())
        {
            if (ae.e1.op == EXP.address && ae.e2.op == EXP.int64)
            {
                AddrExp ade = ae.e1.isAddrExp();
                Expression ex = interpretRegion(ade.e1, istate);
                if (exceptionOrCant(ex))
                    return;
                if (auto se = ex.isStructLiteralExp())
                {
                    dinteger_t offset = ae.e2.toInteger();
                    result = se.getField(e.type, cast(uint)offset);
                    if (result)
                        return;
                }
            }
        }

        // It's possible we have an array bounds error. We need to make sure it
        // errors with this line number, not the one where the pointer was set.
        result = interpretRegion(e.e1, istate);
        if (exceptionOrCant(result))
            return;

        if (result.op == EXP.function_)
            return;
        if (auto soe = result.isSymOffExp())
        {
            if (soe.offset == 0 && soe.var.isFuncDeclaration())
                return;
            if (soe.offset == 0 && soe.var.isVarDeclaration() && soe.var.isImmutable())
            {
                result = getVarExp(e.loc, istate, soe.var, CTFEGoal.RValue);
                return;
            }
            error(e.loc, "cannot dereference pointer to static variable `%s` at compile time", soe.var.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        if (!lvalue && result.isArrayLiteralExp() &&
            result.type.isTypePointer())
        {
            /* A pointer variable can point to an array literal like `[3]`.
             * Dereferencing it means accessing the first element value.
             * Dereference it only if result should be an rvalue
             */
            auto ae = result.isArrayLiteralExp();
            if (ae.elements.length == 1)
            {
                result = (*ae.elements)[0];
                return;
            }
        }
        if (result.isStringExp() || result.isArrayLiteralExp())
            return;

        if (result.op != EXP.address)
        {
            if (result.op == EXP.null_)
                error(e.loc, "dereference of null pointer `%s`", e.e1.toChars());
            else
                error(e.loc, "dereference of invalid pointer `%s`", result.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        // *(&x) ==> x
        result = result.isAddrExp().e1;

        if (result.op == EXP.slice && e.type.toBasetype().ty == Tsarray)
        {
            /* aggr[lwr..upr]
             * upr may exceed the upper boundary of aggr, but the check is deferred
             * until those out-of-bounds elements will be touched.
             */
            return;
        }
        result = interpret(pue, result, istate, goal);
        if (exceptionOrCant(result))
            return;

        debug (LOG)
        {
            if (CTFEExp.isCantExp(result))
                printf("PtrExp::interpret() %s = CTFEExp::cantexp\n", e.toChars());
        }
    }

    override void visit(DotVarExp e)
    {
        void notImplementedYet()
        {
            error(e.loc, "`%s.%s` is not yet implemented at compile time", e.e1.toChars(), e.var.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        debug (LOG)
        {
            printf("%s DotVarExp::interpret() %s, goal = %d\n", e.loc.toChars(), e.toChars(), goal);
        }
        Expression ex = interpretRegion(e.e1, istate);
        if (exceptionOrCant(ex))
            return;

        if (FuncDeclaration f = e.var.isFuncDeclaration())
        {
            if (ex == e.e1)
                result = e; // optimize: reuse this CTFE reference
            else
            {
                emplaceExp!(DotVarExp)(pue, e.loc, ex, f, false);
                result = pue.exp();
                result.type = e.type;
            }
            return;
        }

        VarDeclaration v = e.var.isVarDeclaration();
        if (!v)
        {
            error(e.loc, "CTFE internal error: `%s`", e.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        if (ex.op == EXP.null_)
        {
            if (ex.type.toBasetype().ty == Tclass)
                error(e.loc, "class `%s` is `null` and cannot be dereferenced", e.e1.toChars());
            else
                error(e.loc, "CTFE internal error: null this `%s`", e.e1.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        StructLiteralExp se;
        int i;

        if (ex.op != EXP.structLiteral && ex.op != EXP.classReference && ex.op != EXP.typeid_)
        {
            return notImplementedYet();
        }

        // We can't use getField, because it makes a copy
        if (ex.op == EXP.classReference)
        {
            se = ex.isClassReferenceExp().value;
            i = ex.isClassReferenceExp().findFieldIndexByName(v);
        }
        else if (ex.op == EXP.typeid_)
        {
            if (v.ident == Identifier.idPool("name"))
            {
                if (auto t = isType(ex.isTypeidExp().obj))
                {
                    import dmd.typesem : toDsymbol;
                    auto sym = t.toDsymbol(null);
                    if (auto ident = (sym ? sym.ident : null))
                    {
                        result = new StringExp(e.loc, ident.toString());
                        result.expressionSemantic(null);
                        return ;
                    }
                }
            }
            return notImplementedYet();
        }
        else
        {
            se = ex.isStructLiteralExp();
            i = findFieldIndexByName(se.sd, v);
        }
        if (i == -1)
        {
            error(e.loc, "couldn't find field `%s` of type `%s` in `%s`", v.toChars(), e.type.toChars(), se.toChars());
            result = CTFEExp.cantexp;
            return;
        }

        // https://issues.dlang.org/show_bug.cgi?id=19897
        // https://issues.dlang.org/show_bug.cgi?id=20710
        // Zero-elements fields don't have an initializer. See: scrubArray function
        if ((*se.elements)[i] is null)
            (*se.elements)[i] = voidInitLiteral(e.type, v).copy();

        if (goal == CTFEGoal.LValue)
        {
            // just return the (simplified) dotvar expression as a CTFE reference
            if (e.e1 == ex)
                result = e;
            else
            {
                emplaceExp!(DotVarExp)(pue, e.loc, ex, v);
                result = pue.exp();
                result.type = e.type;
            }
            return;
        }

        result = (*se.elements)[i];
        if (!result)
        {
            error(e.loc, "internal compiler error: null field `%s`", v.toChars());
            result = CTFEExp.cantexp;
            return;
        }
        if (auto vie = result.isVoidInitExp())
        {
            const s = vie.var.toChars();
            if (v.overlapped)
            {
                error(e.loc, "reinterpretation through overlapped field `%s` is not allowed in CTFE", s);
                result = CTFEExp.cantexp;
                return;
            }
            error(e.loc, "cannot read uninitialized variable `%s` in CTFE", s);
            result = CTFEExp.cantexp;
            return;
        }

        if (v.type.ty != result.type.ty && v.type.ty == Tsarray)
        {
            // Block assignment from inside struct literals
            auto tsa = cast(TypeSArray)v.type;
            auto len = cast(size_t)tsa.dim.toInteger();
            UnionExp ue = void;
            result = createBlockDuplicatedArrayLiteral(&ue, e.loc, v.type, result, len);
            if (result == ue.exp())
                result = ue.copy();
            (*se.elements)[i] = result;
        }
        debug (LOG)
        {
            if (CTFEExp.isCantExp(result))
                printf("DotVarExp::interpret() %s = CTFEExp::cantexp\n", e.toChars());
        }
    }

    override void visit(RemoveExp e)
    {
        debug (LOG)
        {
            printf("%s RemoveExp::interpret() %s\n", e.loc.toChars(), e.toChars());
        }
        Expression agg = interpret(e.e1, istate);
        if (exceptionOrCant(agg))
            return;
        Expression index = interpret(e.e2, istate);
        if (exceptionOrCant(index))
            return;
        if (agg.op == EXP.null_)
        {
            result = CTFEExp.voidexp;
            return;
        }

        AssocArrayLiteralExp aae = agg.isAssocArrayLiteralExp();
        Expressions* keysx = aae.keys;
        Expressions* valuesx = aae.values;
        size_t removed = 0;
        foreach (j, evalue; *valuesx)
        {
            Expression ekey = (*keysx)[j];
            int eq = ctfeEqual(e.loc, EXP.equal, ekey, index);
            if (eq)
                ++removed;
            else if (removed != 0)
            {
                (*keysx)[j - removed] = ekey;
                (*valuesx)[j - removed] = evalue;
            }
        }
        valuesx.length = valuesx.length - removed;
        keysx.length = keysx.length - removed;
        result = IntegerExp.createBool(removed != 0);
    }

    override void visit(ClassReferenceExp e)
    {
        //printf("ClassReferenceExp::interpret() %s\n", e.value.toChars());
        result = e;
    }

    override void visit(VoidInitExp e)
    {
        error(e.loc, "CTFE internal error: trying to read uninitialized variable");
        assert(0);
    }

    override void visit(ThrownExceptionExp e)
    {
        assert(0); // This should never be interpreted
    }
}

/// Interpret `throw <exp>` found at the specified location `loc`
private
void interpretThrow(ref Expression result, Expression exp, Loc loc, InterState* istate)
{
    incUsageCtfe(istate, loc);

    Expression e = interpretRegion(exp, istate);
    if (exceptionOrCantInterpret(e))
    {
        // Make sure e is not pointing to a stack temporary
        result = (e.op == EXP.cantExpression) ? CTFEExp.cantexp : e;
    }
    else if (e.op == EXP.classReference)
    {
        result = ctfeEmplaceExp!ThrownExceptionExp(loc, e.isClassReferenceExp());
    }
    else
    {
        error(exp.loc, "to be thrown `%s` must be non-null", exp.toChars());
        result = ErrorExp.get();
    }
}

/*********************************************
 * Checks if the given expresion is a call to the runtime hook `id`.
 *
 * Params:
 *    e = the expression to check
 *    id = the identifier of the runtime hook
 * Returns:
 *    `e` cast to `CallExp` if it's the hook, `null` otherwise
 */
public CallExp isRuntimeHook(Expression e, Identifier id)
{
    if (auto ce = e.isCallExp())
    {
        if (auto ve = ce.e1.isVarExp())
        {
            if (auto fd = ve.var.isFuncDeclaration())
            {
                // If `_d_HookTraceImpl` is found, resolve the underlying hook
                // and replace `e` and `fd` with it.
                removeHookTraceImpl(ce, fd);
                return fd.ident == id ? ce : null;
            }
        }
    }

    return null;
}

/********************************************
 * Interpret the expression.
 * Params:
 *    pue = non-null pointer to temporary storage that can be used to store the return value
 *    e = Expression to interpret
 *    istate = context
 *    goal = what the result will be used for
 * Returns:
 *    resulting expression
 */

Expression interpret(UnionExp* pue, Expression e, InterState* istate, CTFEGoal goal = CTFEGoal.RValue)
{
    if (!e)
        return null;
    //printf("+interpret() e : %s, %s\n", e.type.toChars(), e.toChars());
    scope Interpreter v = new Interpreter(pue, istate, goal);
    e.accept(v);
    Expression ex = v.result;
    assert(goal == CTFEGoal.Nothing || ex !is null);
    //if (ex) printf("-interpret() ex: %s, %s\n", ex.type.toChars(), ex.toChars()); else printf("-interpret()\n");
    return ex;
}

///
Expression interpret(Expression e, InterState* istate, CTFEGoal goal = CTFEGoal.RValue)
{
    UnionExp ue = void;
    auto result = interpret(&ue, e, istate, goal);
    if (result == ue.exp())
        result = ue.copy();
    return result;
}

/*****************************
 * Same as interpret(), but return result allocated in Region.
 * Params:
 *    e = Expression to interpret
 *    istate = context
 *    goal = what the result will be used for
 * Returns:
 *    resulting expression
 */
Expression interpretRegion(Expression e, InterState* istate, CTFEGoal goal = CTFEGoal.RValue)
{
    UnionExp ue = void;
    auto result = interpret(&ue, e, istate, goal);
    auto uexp = ue.exp();
    if (result != uexp)
        return result;
    if (mem.isGCEnabled)
        return ue.copy();

    // mimicking UnionExp.copy, but with region allocation
    switch (uexp.op)
    {
        case EXP.cantExpression: return CTFEExp.cantexp;
        case EXP.voidExpression: return CTFEExp.voidexp;
        case EXP.break_:         return CTFEExp.breakexp;
        case EXP.continue_:      return CTFEExp.continueexp;
        case EXP.goto_:          return CTFEExp.gotoexp;
        default:                 break;
    }
    auto p = ctfeGlobals.region.malloc(uexp.size);
    return cast(Expression)memcpy(p, cast(void*)uexp, uexp.size);
}

private
Expressions* copyArrayOnWrite(Expressions* exps, Expressions* original)
{
    if (exps is original)
    {
        if (!original)
            exps = new Expressions();
        else
            exps = original.copy();
        ++ctfeGlobals.numArrayAllocs;
    }
    return exps;
}

/**
 Given an expression e which is about to be returned from the current
 function, generate an error if it contains pointers to local variables.

 Only checks expressions passed by value (pointers to local variables
 may already be stored in members of classes, arrays, or AAs which
 were passed as mutable function parameters).
 Returns:
    true if it is safe to return, false if an error was generated.
 */
private
bool stopPointersEscaping(Loc loc, Expression e)
{
    import dmd.typesem : hasPointers;
    if (!e.type.hasPointers())
        return true;
    if (isPointer(e.type))
    {
        Expression x = e;
        if (auto eaddr = e.isAddrExp())
            x = eaddr.e1;
        VarDeclaration v;
        while (x.op == EXP.variable && (v = x.isVarExp().var.isVarDeclaration()) !is null)
        {
            if (v.storage_class & STC.ref_)
            {
                x = getValue(v);
                if (auto eaddr = e.isAddrExp())
                    eaddr.e1 = x;
                continue;
            }
            if (ctfeGlobals.stack.isInCurrentFrame(v))
            {
                error(loc, "returning a pointer to a local stack variable");
                return false;
            }
            else
                break;
        }
        // TODO: If it is a EXP.dotVariable or EXP.index, we should check that it is not
        // pointing to a local struct or static array.
    }
    if (auto se = e.isStructLiteralExp())
    {
        return stopPointersEscapingFromArray(loc, se.elements);
    }
    if (auto ale = e.isArrayLiteralExp())
    {
        return stopPointersEscapingFromArray(loc, ale.elements);
    }
    if (auto aae = e.isAssocArrayLiteralExp())
    {
        if (!stopPointersEscapingFromArray(loc, aae.keys))
            return false;
        return stopPointersEscapingFromArray(loc, aae.values);
    }
    return true;
}

// Check all elements of an array for escaping local variables. Return false if error
private
bool stopPointersEscapingFromArray(Loc loc, Expressions* elems)
{
    foreach (e; *elems)
    {
        if (e && !stopPointersEscaping(loc, e))
            return false;
    }
    return true;
}

private
Statement findGotoTarget(InterState* istate, Identifier ident)
{
    Statement target = null;
    if (ident)
    {
        LabelDsymbol label = istate.fd.searchLabel(ident, Loc.initial);
        assert(label && label.statement);
        LabelStatement ls = label.statement;
        target = ls.gotoTarget ? ls.gotoTarget : ls.statement;
    }
    return target;
}

private
ThrownExceptionExp chainExceptions(ThrownExceptionExp oldest, ThrownExceptionExp newest)
{
    debug (LOG)
    {
        printf("Collided exceptions %s %s\n", oldest.thrown.toChars(), newest.thrown.toChars());
    }
    // Little sanity check to make sure it's really a Throwable
    ClassReferenceExp boss = oldest.thrown;
    const next = 5;                         // index of Throwable.next
    assert((*boss.value.elements)[next].type.ty == Tclass); // Throwable.next
    ClassReferenceExp collateral = newest.thrown;
    if (collateral.originalClass().isErrorException() && !boss.originalClass().isErrorException())
    {
        /* Find the index of the Error.bypassException field
         */
        auto bypass = next + 1;
        if ((*collateral.value.elements)[bypass].type.ty == Tuns32)
            bypass += 1;  // skip over _refcount field
        assert((*collateral.value.elements)[bypass].type.ty == Tclass);

        // The new exception bypass the existing chain
        (*collateral.value.elements)[bypass] = boss;
        return newest;
    }
    while ((*boss.value.elements)[next].op == EXP.classReference)
    {
        boss = (*boss.value.elements)[next].isClassReferenceExp();
    }
    (*boss.value.elements)[next] = collateral;
    return oldest;
}

/**
 * All results destined for use outside of CTFE need to have their CTFE-specific
 * features removed.
 * In particular,
 * 1. all slices must be resolved.
 * 2. all .ownedByCtfe set to OwnedBy.code
 */
private Expression scrubReturnValue(Loc loc, Expression e)
{
    /* Returns: true if e is void,
     * or is an array literal or struct literal of void elements.
     */
    static bool isVoid(const Expression e, bool checkArrayType = false) pure
    {
        if (e.op == EXP.void_)
            return true;

        static bool isEntirelyVoid(const Expressions* elems)
        {
            foreach (e; *elems)
            {
                // It can be NULL for performance reasons,
                // see StructLiteralExp::interpret().
                if (e && !isVoid(e))
                    return false;
            }
            return true;
        }

        if (auto sle = e.isStructLiteralExp())
            return isEntirelyVoid(sle.elements);

        if (checkArrayType && e.type.ty != Tsarray)
            return false;

        if (auto ale = e.isArrayLiteralExp())
            return isEntirelyVoid(ale.elements);

        return false;
    }


    /* Scrub all elements of elems[].
     * Returns: null for success, error Expression for failure
     */
    Expression scrubArray(Expressions* elems, bool structlit = false)
    {
        foreach (ref e; *elems)
        {
            // It can be NULL for performance reasons,
            // see StructLiteralExp::interpret().
            if (!e)
                continue;

            // A struct .init may contain void members.
            // Static array members are a weird special case https://issues.dlang.org/show_bug.cgi?id=10994
            if (structlit && isVoid(e, true))
            {
                e = null;
            }
            else
            {
                e = scrubReturnValue(loc, e);
                if (CTFEExp.isCantExp(e) || e.op == EXP.error)
                    return e;
            }
        }
        return null;
    }

    Expression scrubSE(StructLiteralExp sle)
    {
        sle.ownedByCtfe = OwnedBy.code;
        if (!(sle.stageflags & StructLiteralExp.StageFlags.scrub))
        {
            const old = sle.stageflags;
            sle.stageflags |= StructLiteralExp.StageFlags.scrub; // prevent infinite recursion
            if (auto ex = scrubArray(sle.elements, true))
                return ex;
            sle.stageflags = old;
        }
        return null;
    }

    if (e.op == EXP.classReference)
    {
        StructLiteralExp sle = e.isClassReferenceExp().value;
        if (auto ex = scrubSE(sle))
            return ex;
    }
    else if (auto vie = e.isVoidInitExp())
    {
        error(loc, "uninitialized variable `%s` cannot be returned from CTFE", vie.var.toChars());
        return ErrorExp.get();
    }

    e = resolveSlice(e);

    if (auto sle = e.isStructLiteralExp())
    {
        if (auto ex = scrubSE(sle))
            return ex;
    }
    else if (auto se = e.isStringExp())
    {
        se.ownedByCtfe = OwnedBy.code;
    }
    else if (auto ale = e.isArrayLiteralExp())
    {
        ale.ownedByCtfe = OwnedBy.code;
        if (auto ex = scrubArray(ale.elements))
            return ex;
    }
    else if (auto aae = e.isAssocArrayLiteralExp())
    {
        aae.ownedByCtfe = OwnedBy.code;
        if (auto ex = scrubArray(aae.keys))
            return ex;
        if (auto ex = scrubArray(aae.values))
            return ex;
        aae.type = toBuiltinAAType(aae.type);
    }
    else if (auto ve = e.isVectorExp())
    {
        ve.ownedByCtfe = OwnedBy.code;
        if (auto ale = ve.e1.isArrayLiteralExp())
        {
            ale.ownedByCtfe = OwnedBy.code;
            if (auto ex = scrubArray(ale.elements))
                return ex;
        }
    }
    return e;
}

/**************************************
 * Transitively set all .ownedByCtfe to OwnedBy.cache
 */
private Expression scrubCacheValue(Expression e)
{
    if (!e)
        return e;

    Expression scrubArrayCache(Expressions* elems)
    {
        foreach (ref e; *elems)
            e = scrubCacheValue(e);
        return null;
    }

    Expression scrubSE(StructLiteralExp sle)
    {
        sle.ownedByCtfe = OwnedBy.cache;
        if (!(sle.stageflags & StructLiteralExp.StageFlags.scrub))
        {
            const old = sle.stageflags;
            sle.stageflags |= StructLiteralExp.StageFlags.scrub;  // prevent infinite recursion
            if (auto ex = scrubArrayCache(sle.elements))
                return ex;
            sle.stageflags = old;
        }
        return null;
    }

    if (e.op == EXP.classReference)
    {
        if (auto ex = scrubSE(e.isClassReferenceExp().value))
            return ex;
    }
    else if (auto sle = e.isStructLiteralExp())
    {
        if (auto ex = scrubSE(sle))
            return ex;
    }
    else if (auto se = e.isStringExp())
    {
        se.ownedByCtfe = OwnedBy.cache;
    }
    else if (auto ale = e.isArrayLiteralExp())
    {
        ale.ownedByCtfe = OwnedBy.cache;
        if (Expression ex = scrubArrayCache(ale.elements))
            return ex;
    }
    else if (auto aae = e.isAssocArrayLiteralExp())
    {
        aae.ownedByCtfe = OwnedBy.cache;
        if (auto ex = scrubArrayCache(aae.keys))
            return ex;
        if (auto ex = scrubArrayCache(aae.values))
            return ex;
    }
    else if (auto ve = e.isVectorExp())
    {
        ve.ownedByCtfe = OwnedBy.cache;
        if (auto ale = ve.e1.isArrayLiteralExp())
        {
            ale.ownedByCtfe = OwnedBy.cache;
            if (auto ex = scrubArrayCache(ale.elements))
                return ex;
        }
    }
    return e;
}

/********************************************
 * Transitively replace all Expressions allocated in ctfeGlobals.region
 * with Mem owned copies.
 * Params:
 *      e = possible ctfeGlobals.region owned expression
 * Returns:
 *      Mem owned expression
 */
private Expression copyRegionExp(Expression e)
{
    if (!e)
        return e;

    static void copyArray(Expressions* elems)
    {
        foreach (ref e; *elems)
        {
            auto ex = e;
            e = null;
            e = copyRegionExp(ex);
        }
    }

    static void copySE(StructLiteralExp sle)
    {
        if (1 || !(sle.stageflags & StructLiteralExp.StageFlags.scrub))
        {
            const old = sle.stageflags;
            sle.stageflags |= StructLiteralExp.StageFlags.scrub; // prevent infinite recursion
            copyArray(sle.elements);
            sle.stageflags = old;
        }
    }

    switch (e.op)
    {
        case EXP.classReference:
        {
            auto cre = e.isClassReferenceExp();
            cre.value = copyRegionExp(cre.value).isStructLiteralExp();
            break;
        }

        case EXP.structLiteral:
        {
            auto sle = e.isStructLiteralExp();

            /* The following is to take care of updating sle.origin correctly,
             * which may have multiple objects pointing to it.
             */
            if (sle.isOriginal && !ctfeGlobals.region.contains(cast(void*)sle.origin))
            {
                /* This means sle has already been moved out of the region,
                 * and sle.origin is the new location.
                 */
                return sle.origin;
            }
            copySE(sle);
            sle.isOriginal = sle is sle.origin;

            auto slec = ctfeGlobals.region.contains(cast(void*)e)
                ? e.copy().isStructLiteralExp()         // move sle out of region to slec
                : sle;

            if (ctfeGlobals.region.contains(cast(void*)sle.origin))
            {
                auto sleo = sle.origin == sle ? slec : sle.origin.copy().isStructLiteralExp();
                sle.origin = sleo;
                slec.origin = sleo;
            }
            return slec;
        }

        case EXP.arrayLiteral:
        {
            auto ale = e.isArrayLiteralExp();
            ale.basis = copyRegionExp(ale.basis);
            copyArray(ale.elements);
            break;
        }

        case EXP.assocArrayLiteral:
            copyArray(e.isAssocArrayLiteralExp().keys);
            copyArray(e.isAssocArrayLiteralExp().values);
            break;

        case EXP.slice:
        {
            auto se = e.isSliceExp();
            se.e1  = copyRegionExp(se.e1);
            se.upr = copyRegionExp(se.upr);
            se.lwr = copyRegionExp(se.lwr);
            break;
        }

        case EXP.tuple:
        {
            auto te = e.isTupleExp();
            te.e0 = copyRegionExp(te.e0);
            copyArray(te.exps);
            break;
        }

        case EXP.address:
        case EXP.delegate_:
        case EXP.vector:
        case EXP.dotVariable:
        {
            UnaExp ue = e.isUnaExp();
            ue.e1 = copyRegionExp(ue.e1);
            break;
        }

        case EXP.index:
        {
            BinExp be = e.isBinExp();
            be.e1 = copyRegionExp(be.e1);
            be.e2 = copyRegionExp(be.e2);
            break;
        }

        case EXP.this_:
        case EXP.super_:
        case EXP.variable:
        case EXP.type:
        case EXP.function_:
        case EXP.typeid_:
        case EXP.string_:
        case EXP.int64:
        case EXP.error:
        case EXP.float64:
        case EXP.complex80:
        case EXP.null_:
        case EXP.void_:
        case EXP.symbolOffset:
            break;

        case EXP.cantExpression:
        case EXP.voidExpression:
        case EXP.showCtfeContext:
            return e;

        default:
            printf("e: %s, %s\n", EXPtoString(e.op).ptr, e.toChars());
            assert(0);
    }

    if (ctfeGlobals.region.contains(cast(void*)e))
    {
        return e.copy();
    }
    return e;
}

/******************************* Special Functions ***************************/

private Expression interpret_length(UnionExp* pue, InterState* istate, Expression earg)
{
    //printf("interpret_length()\n");
    earg = interpret(pue, earg, istate);
    if (exceptionOrCantInterpret(earg))
        return earg;
    dinteger_t len = 0;
    if (auto aae = earg.isAssocArrayLiteralExp())
        len = aae.keys.length;
    else
        assert(earg.op == EXP.null_);
    emplaceExp!(IntegerExp)(pue, earg.loc, len, Type.tsize_t);
    return pue.exp();
}

private Expression interpret_keys(UnionExp* pue, InterState* istate, Expression earg, Type returnType)
{
    debug (LOG)
    {
        printf("interpret_keys()\n");
    }
    earg = interpret(pue, earg, istate);
    if (exceptionOrCantInterpret(earg))
        return earg;
    if (earg.op == EXP.null_)
    {
        emplaceExp!(NullExp)(pue, earg.loc, earg.type);
        return pue.exp();
    }
    if (earg.op != EXP.assocArrayLiteral && earg.type.toBasetype().ty != Taarray)
        return null;
    AssocArrayLiteralExp aae = earg.isAssocArrayLiteralExp();
    auto ae = ctfeEmplaceExp!ArrayLiteralExp(aae.loc, returnType, aae.keys);
    ae.ownedByCtfe = aae.ownedByCtfe;
    *pue = copyLiteral(ae);
    return pue.exp();
}

private Expression interpret_values(UnionExp* pue, InterState* istate, Expression earg, Type returnType)
{
    debug (LOG)
    {
        printf("interpret_values()\n");
    }
    earg = interpret(pue, earg, istate);
    if (exceptionOrCantInterpret(earg))
        return earg;
    if (earg.op == EXP.null_)
    {
        emplaceExp!(NullExp)(pue, earg.loc, earg.type);
        return pue.exp();
    }
    if (earg.op != EXP.assocArrayLiteral && earg.type.toBasetype().ty != Taarray)
        return null;
    auto aae = earg.isAssocArrayLiteralExp();
    auto ae = ctfeEmplaceExp!ArrayLiteralExp(aae.loc, returnType, aae.values);
    ae.ownedByCtfe = aae.ownedByCtfe;
    //printf("result is %s\n", e.toChars());
    *pue = copyLiteral(ae);
    return pue.exp();
}

private Expression interpret_dup(UnionExp* pue, InterState* istate, Expression earg)
{
    debug (LOG)
    {
        printf("interpret_dup()\n");
    }
    earg = interpret(pue, earg, istate);
    if (exceptionOrCantInterpret(earg))
        return earg;
    if (earg.op == EXP.null_)
    {
        emplaceExp!(NullExp)(pue, earg.loc, earg.type);
        return pue.exp();
    }
    if (earg.op != EXP.assocArrayLiteral && earg.type.toBasetype().ty != Taarray)
        return null;
    auto aae = copyLiteral(earg).copy().isAssocArrayLiteralExp();
    for (size_t i = 0; i < aae.keys.length; i++)
    {
        if (Expression e = evaluatePostblit(istate, (*aae.keys)[i]))
            return e;
        if (Expression e = evaluatePostblit(istate, (*aae.values)[i]))
            return e;
    }
    aae.type = earg.type.mutableOf(); // repaint type from const(int[int]) to const(int)[int]
    //printf("result is %s\n", aae.toChars());
    return aae;
}

// signature is int delegate(ref Value) OR int delegate(ref Key, ref Value)
private Expression interpret_aaApply(UnionExp* pue, InterState* istate, Expression aa, Expression deleg)
{
    aa = interpret(aa, istate);
    if (exceptionOrCantInterpret(aa))
        return aa;
    if (aa.op != EXP.assocArrayLiteral)
    {
        emplaceExp!(IntegerExp)(pue, deleg.loc, 0, Type.tsize_t);
        return pue.exp();
    }

    FuncDeclaration fd = null;
    Expression pthis = null;
    if (auto de = deleg.isDelegateExp())
    {
        fd = de.func;
        pthis = de.e1;
    }
    else if (auto fe = deleg.isFuncExp())
        fd = fe.fd;

    assert(fd && fd.fbody);
    assert(fd.parameters);
    size_t numParams = fd.parameters.length;
    assert(numParams == 1 || numParams == 2);

    Parameter fparam = fd.type.isTypeFunction().parameterList[numParams - 1];
    const wantRefValue = fparam.isReference();

    Expressions args = Expressions(numParams);

    AssocArrayLiteralExp ae = aa.isAssocArrayLiteralExp();
    if (!ae.keys || ae.keys.length == 0)
        return ctfeEmplaceExp!IntegerExp(deleg.loc, 0, Type.tsize_t);
    Expression eresult;

    for (size_t i = 0; i < ae.keys.length; ++i)
    {
        Expression ekey = (*ae.keys)[i];
        Expression evalue = (*ae.values)[i];
        if (wantRefValue)
        {
            Type t = evalue.type;
            evalue = ctfeEmplaceExp!IndexExp(deleg.loc, ae, ekey);
            evalue.type = t;
        }
        args[numParams - 1] = evalue;
        if (numParams == 2)
            args[0] = ekey;

        UnionExp ue = void;
        eresult = interpretFunction(&ue, fd, istate, &args, pthis);
        if (eresult == ue.exp())
            eresult = ue.copy();
        if (exceptionOrCantInterpret(eresult))
            return eresult;

        if (eresult.isIntegerExp().getInteger() != 0)
            return eresult;
    }
    return eresult;
}

/// Returns: equivalent `StringExp` from `ArrayLiteralExp ale` containing only `IntegerExp` elements
StringExp arrayLiteralToString(ArrayLiteralExp ale)
{
    const len = ale.elements ? ale.elements.length : 0;
    const size = ale.type.nextOf().size();

    StringExp impl(T)()
    {
        T[] result = new T[len];
        foreach (i; 0 .. len)
            result[i] = cast(T) (*ale.elements)[i].isIntegerExp().getInteger();
        return new StringExp(ale.loc, result[], len, cast(ubyte) size);
    }

    switch (size)
    {
        case 1:
            return impl!char();
        case 2:
            return impl!wchar();
        case 4:
            return impl!dchar();
        default:
            assert(0);
    }
}

/* Decoding UTF strings for foreach loops. Duplicates the functionality of
 * the twelve _aApplyXXn functions in aApply.d in the runtime.
 */
private Expression foreachApplyUtf(UnionExp* pue, InterState* istate, Expression str, Expression deleg, bool rvs)
{
    debug (LOG)
    {
        printf("foreachApplyUtf(%s, %s)\n", str.toChars(), deleg.toChars());
    }
    FuncDeclaration fd = null;
    Expression pthis = null;
    if (auto de = deleg.isDelegateExp())
    {
        fd = de.func;
        pthis = de.e1;
    }
    else if (auto fe = deleg.isFuncExp())
        fd = fe.fd;

    assert(fd && fd.fbody);
    assert(fd.parameters);
    size_t numParams = fd.parameters.length;
    assert(numParams == 1 || numParams == 2);
    Type charType = (*fd.parameters)[numParams - 1].type;
    Type indexType = numParams == 2 ? (*fd.parameters)[0].type : Type.tsize_t;
    size_t len = cast(size_t)resolveArrayLength(str);
    if (len == 0)
    {
        emplaceExp!(IntegerExp)(pue, deleg.loc, 0, indexType);
        return pue.exp();
    }

    UnionExp strTmp = void;
    str = resolveSlice(str, &strTmp);

    auto se = str.isStringExp();
    if (auto ale = str.isArrayLiteralExp())
        se = arrayLiteralToString(ale);

    if (!se)
    {
        error(str.loc, "CTFE internal error: cannot foreach `%s`", str.toChars());
        return CTFEExp.cantexp;
    }
    Expressions args = Expressions(numParams);

    Expression eresult = null; // ded-store to prevent spurious warning

    // Buffers for encoding
    char[4] utf8buf = void;
    wchar[2] utf16buf = void;

    size_t start = rvs ? len : 0;
    size_t end = rvs ? 0 : len;
    for (size_t indx = start; indx != end;)
    {
        // Step 1: Decode the next dchar from the string.

        string errmsg = null; // Used for reporting decoding errors
        dchar rawvalue; // Holds the decoded dchar
        size_t currentIndex = indx; // The index of the decoded character

        // String literals
        size_t saveindx; // used for reverse iteration

        switch (se.sz)
        {
            case 1:
            {
                if (rvs)
                {
                    // find the start of the string
                    --indx;
                    while (indx > 0 && ((se.getCodeUnit(indx) & 0xC0) == 0x80))
                        --indx;
                    saveindx = indx;
                }
                auto slice = se.peekString();
                errmsg = utf_decodeChar(slice, indx, rawvalue);
                if (rvs)
                    indx = saveindx;
                break;
            }

            case 2:
                if (rvs)
                {
                    // find the start
                    --indx;
                    auto wc = se.getCodeUnit(indx);
                    if (wc >= 0xDC00 && wc <= 0xDFFF)
                        --indx;
                    saveindx = indx;
                }
                const slice = se.peekWstring();
                errmsg = utf_decodeWchar(slice, indx, rawvalue);
                if (rvs)
                    indx = saveindx;
                break;

            case 4:
                if (rvs)
                    --indx;
                rawvalue = se.getCodeUnit(indx);
                if (!rvs)
                    ++indx;
                break;

            default:
                assert(0);
        }

        if (errmsg)
        {
            error(deleg.loc, "`%.*s`", cast(int)errmsg.length, errmsg.ptr);
            return CTFEExp.cantexp;
        }

        // Step 2: encode the dchar in the target encoding

        int charlen = 1; // How many codepoints are involved?
        switch (charType.size())
        {
        case 1:
            charlen = utf_codeLengthChar(rawvalue);
            utf_encodeChar(&utf8buf[0], rawvalue);
            break;
        case 2:
            charlen = utf_codeLengthWchar(rawvalue);
            utf_encodeWchar(&utf16buf[0], rawvalue);
            break;
        case 4:
            break;
        default:
            assert(0);
        }
        if (rvs)
            currentIndex = indx;

        // Step 3: call the delegate once for each code point

        // The index only needs to be set once
        if (numParams == 2)
            args[0] = ctfeEmplaceExp!IntegerExp(deleg.loc, currentIndex, indexType);

        Expression val = null;

        foreach (k; 0 .. charlen)
        {
            dchar codepoint;
            switch (charType.size())
            {
            case 1:
                codepoint = utf8buf[k];
                break;
            case 2:
                codepoint = utf16buf[k];
                break;
            case 4:
                codepoint = rawvalue;
                break;
            default:
                assert(0);
            }
            val = ctfeEmplaceExp!IntegerExp(str.loc, codepoint, charType);

            args[numParams - 1] = val;

            UnionExp ue = void;
            eresult = interpretFunction(&ue, fd, istate, &args, pthis);
            if (eresult == ue.exp())
                eresult = ue.copy();
            if (exceptionOrCantInterpret(eresult))
                return eresult;
            if (eresult.isIntegerExp().getInteger() != 0)
                return eresult;
        }
    }
    return eresult;
}

/* If this is a built-in function, return the interpreted result,
 * Otherwise, return NULL.
 */
private Expression evaluateIfBuiltin(UnionExp* pue, InterState* istate, Loc loc, FuncDeclaration fd, Expressions* arguments, Expression pthis)
{
    Expression e = null;
    size_t nargs = arguments ? arguments.length : 0;
    if (!pthis)
    {
        if (isBuiltin(fd) != BUILTIN.unimp)
        {
            Expressions args = Expressions(nargs);
            foreach (i, ref arg; args)
            {
                Expression earg = (*arguments)[i];
                earg = interpret(earg, istate);
                if (exceptionOrCantInterpret(earg))
                    return earg;
                arg = earg;
            }
            e = eval_builtin(loc, fd, &args);
            if (!e)
            {
                error(loc, "cannot evaluate unimplemented builtin `%s` at compile time", fd.toChars());
                e = CTFEExp.cantexp;
            }
        }
    }
    if (!pthis)
    {
        if (nargs == 1 || nargs == 3)
        {
            Expression firstarg = (*arguments)[0];
            if (auto firstAAtype = firstarg.type.toBasetype().isTypeAArray())
            {
                const id = fd.ident;
                if (nargs == 1)
                {
                    if (id == Id.aaLen)
                        return interpret_length(pue, istate, firstarg);

                    if (fd.toParent2().ident == Id.object)
                    {
                        if (id == Id.keys)
                            return interpret_keys(pue, istate, firstarg, firstAAtype.index.arrayOf());
                        if (id == Id.values)
                            return interpret_values(pue, istate, firstarg, firstAAtype.nextOf().arrayOf());
                        if (id == Id.rehash)
                            return interpret(pue, firstarg, istate);
                        if (id == Id.dup)
                            return interpret_dup(pue, istate, firstarg);
                    }
                }
                else // (nargs == 3)
                {
                    if (id == Id._aaApply)
                        return interpret_aaApply(pue, istate, firstarg, (*arguments)[2]);
                    if (id == Id._aaApply2)
                        return interpret_aaApply(pue, istate, firstarg, (*arguments)[2]);
                }
            }
        }
    }
    if (pthis && !fd.fbody && fd.isCtorDeclaration() && fd.parent && fd.parent.parent && fd.parent.parent.ident == Id.object)
    {
        if (pthis.op == EXP.classReference && fd.parent.ident == Id.Throwable)
        {
            // At present, the constructors just copy their arguments into the struct.
            // But we might need some magic if stack tracing gets added to druntime.
            StructLiteralExp se = pthis.isClassReferenceExp().value;
            assert(arguments.length <= se.elements.length);
            foreach (i, arg; *arguments)
            {
                auto elem = interpret(arg, istate);
                if (exceptionOrCantInterpret(elem))
                    return elem;
                (*se.elements)[i] = elem;
            }
            return CTFEExp.voidexp;
        }
    }
    if (nargs == 1 && !pthis && (fd.ident == Id.criticalenter || fd.ident == Id.criticalexit))
    {
        // Support synchronized{} as a no-op
        return CTFEExp.voidexp;
    }
    if (!pthis)
    {
        const idlen = fd.ident.toString().length;
        const id = fd.ident.toChars();
        if (nargs == 2 && (idlen == 10 || idlen == 11) && !strncmp(id, "_aApply", 7))
        {
            // Functions from aApply.d and aApplyR.d in the runtime
            bool rvs = (idlen == 11); // true if foreach_reverse
            char c = id[idlen - 3]; // char width: 'c', 'w', or 'd'
            char s = id[idlen - 2]; // string width: 'c', 'w', or 'd'
            char n = id[idlen - 1]; // numParams: 1 or 2.
            // There are 12 combinations
            if ((n == '1' || n == '2') &&
                (c == 'c' || c == 'w' || c == 'd') &&
                (s == 'c' || s == 'w' || s == 'd') &&
                c != s)
            {
                Expression str = (*arguments)[0];
                str = interpret(str, istate);
                if (exceptionOrCantInterpret(str))
                    return str;
                return foreachApplyUtf(pue, istate, str, (*arguments)[1], rvs);
            }
        }
    }
    return e;
}

private Expression evaluatePostblit(InterState* istate, Expression e)
{
    auto ts = e.type.baseElemOf().isTypeStruct();
    if (!ts)
        return null;
    StructDeclaration sd = ts.sym;
    if (!sd.postblit)
        return null;

    if (auto ale = e.isArrayLiteralExp())
    {
        foreach (elem; *ale.elements)
        {
            if (auto ex = evaluatePostblit(istate, elem))
                return ex;
        }
        return null;
    }
    if (e.op == EXP.structLiteral)
    {
        // e.__postblit()
        UnionExp ue = void;
        e = interpretFunction(&ue, sd.postblit, istate, null, e);
        if (e == ue.exp())
            e = ue.copy();
        if (exceptionOrCantInterpret(e))
            return e;
        return null;
    }
    assert(0);
}

private Expression evaluateDtor(InterState* istate, Expression e)
{
    auto ts = e.type.baseElemOf().isTypeStruct();
    if (!ts)
        return null;
    StructDeclaration sd = ts.sym;
    if (!sd.dtor)
        return null;

    UnionExp ue = void;
    if (auto ale = e.isArrayLiteralExp())
    {
        foreach_reverse (elem; *ale.elements)
            e = evaluateDtor(istate, elem);
    }
    else if (e.op == EXP.structLiteral)
    {
        // e.__dtor()
        e = interpretFunction(&ue, sd.dtor, istate, null, e);
    }
    else
        assert(0);
    if (exceptionOrCantInterpret(e))
    {
        if (e == ue.exp())
            e = ue.copy();
        return e;
    }
    return null;
}

/*************************** CTFE Sanity Checks ***************************/
/* Setter functions for CTFE variable values.
 * These functions exist to check for compiler CTFE bugs.
 */
private bool hasValue(VarDeclaration vd)
{
    return vd.ctfeAdrOnStack != VarDeclaration.AdrOnStackNone &&
           getValue(vd) !is null;
}

// Don't check for validity
private void setValueWithoutChecking(VarDeclaration vd, Expression newval)
{
    ctfeGlobals.stack.setValue(vd, newval);
}

private void setValue(VarDeclaration vd, Expression newval)
{
    //printf("setValue() vd: %s newval: %s\n", vd.toChars(), newval.toChars());
    version (none)
    {
        if (!((vd.storage_class & (STC.out_ | STC.ref_)) ? isCtfeReferenceValid(newval) : isCtfeValueValid(newval)))
        {
            printf("[%s] vd = %s %s, newval = %s\n", vd.loc.toChars(), vd.type.toChars(), vd.toChars(), newval.toChars());
        }
    }
    assert((vd.storage_class & (STC.out_ | STC.ref_)) ? isCtfeReferenceValid(newval) : isCtfeValueValid(newval));
    ctfeGlobals.stack.setValue(vd, newval);
}

/**
 * Removes `_d_HookTraceImpl` if found from `ce` and `fd`.
 * This is needed for the CTFE interception code to be able to find hooks that are called though the hook's `*Trace`
 * wrapper.
 *
 * This is done by replacing `_d_HookTraceImpl!(T, Hook, errMsg)(..., parameters)` with `Hook(parameters)`.
 * Parameters:
 *  ce = The CallExp that possible will be be replaced
 *  fd = Fully resolve function declaration that `ce` would call
 */
private void removeHookTraceImpl(ref CallExp ce, ref FuncDeclaration fd)
{
    if (fd.ident != Id._d_HookTraceImpl)
        return;

    auto oldCE = ce;

    // Get the Hook from the second template parameter
    TemplateInstance templateInstance = fd.parent.isTemplateInstance;
    RootObject hook = (*templateInstance.tiargs)[1];
    assert(hook.isDsymbol(), "Expected _d_HookTraceImpl's second template parameter to be an alias to the hook!");
    fd = (cast(Dsymbol)hook).isFuncDeclaration;

    // Remove the first three trace parameters
    auto arguments = new Expressions();
    arguments.reserve(ce.arguments.length - 3);
    arguments.pushSlice((*ce.arguments)[3 .. $]);

    ce = ctfeEmplaceExp!CallExp(ce.loc, ctfeEmplaceExp!VarExp(ce.loc, fd, false), arguments);

    if (global.params.v.verbose)
        message("strip     %s =>\n          %s", oldCE.toChars(), ce.toChars());
}
