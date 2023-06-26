/**
 * Most of the logic to implement scoped pointers and scoped references is here.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/escape.d, _escape.d)
 * Documentation:  https://dlang.org/phobos/dmd_escape.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/escape.d
 */

module dmd.escape;

import core.stdc.stdio : printf;
import core.stdc.stdlib;
import core.stdc.string;

import dmd.root.rmem;

import dmd.aggregate;
import dmd.astenums;
import dmd.declaration;
import dmd.dscope;
import dmd.dsymbol;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.printast;
import dmd.root.rootobject;
import dmd.tokens;
import dmd.visitor;
import dmd.arraytypes;

/// Groups global state for escape checking together
package(dmd) struct EscapeState
{
    // Maps `sequenceNumber` of a `VarDeclaration` to an object that contains the
    // reason it failed to infer `scope`
    // https://issues.dlang.org/show_bug.cgi?id=23295
    private __gshared RootObject[int] scopeInferFailure;

    /// Called by `initDMD` / `deinitializeDMD` to reset global state
    static void reset()
    {
        scopeInferFailure = null;
    }
}

/******************************************************
 * Checks memory objects passed to a function.
 * Checks that if a memory object is passed by ref or by pointer,
 * all of the refs or pointers are const, or there is only one mutable
 * ref or pointer to it.
 * References:
 *      DIP 1021
 * Params:
 *      sc = used to determine current function and module
 *      fd = function being called
 *      tf = fd's type
 *      ethis = if not null, the `this` pointer
 *      arguments = actual arguments to function
 *      gag = do not print error messages
 * Returns:
 *      `true` if error
 */
bool checkMutableArguments(Scope* sc, FuncDeclaration fd, TypeFunction tf,
    Expression ethis, Expressions* arguments, bool gag)
{
    enum log = false;
    if (log) printf("[%s] checkMutableArguments, fd: `%s`\n", fd.loc.toChars(), fd.toChars());
    if (log && ethis) printf("ethis: `%s`\n", ethis.toChars());
    bool errors = false;

    /* Outer variable references are treated as if they are extra arguments
     * passed by ref to the function (which they essentially are via the static link).
     */
    VarDeclaration[] outerVars = fd ? fd.outerVars[] : null;

    const len = arguments.length + (ethis !is null) + outerVars.length;
    if (len <= 1)
        return errors;

    struct EscapeBy
    {
        EscapeByResults er;
        Parameter param;        // null if no Parameter for this argument
        bool isMutable;         // true if reference to mutable
    }

    auto escapeBy = new EscapeBy[len];
    const paramLength = tf.parameterList.length;

    // Fill in escapeBy[] with arguments[], ethis, and outerVars[]
    foreach (const i, ref eb; escapeBy)
    {
        bool refs;
        Expression arg;
        if (i < arguments.length)
        {
            arg = (*arguments)[i];
            if (i < paramLength)
            {
                eb.param = tf.parameterList[i];
                refs = eb.param.isReference();
                eb.isMutable = eb.param.isReferenceToMutable(arg.type);
            }
            else
            {
                eb.param = null;
                refs = false;
                eb.isMutable = arg.type.isReferenceToMutable();
            }
        }
        else if (ethis)
        {
            /* ethis is passed by value if a class reference,
             * by ref if a struct value
             */
            eb.param = null;
            arg = ethis;
            auto ad = fd.isThis();
            assert(ad);
            assert(ethis);
            if (ad.isClassDeclaration())
            {
                refs = false;
                eb.isMutable = arg.type.isReferenceToMutable();
            }
            else
            {
                assert(ad.isStructDeclaration());
                refs = true;
                eb.isMutable = arg.type.isMutable();
            }
        }
        else
        {
            // outer variables are passed by ref
            eb.param = null;
            refs = true;
            auto var = outerVars[i - (len - outerVars.length)];
            eb.isMutable = var.type.isMutable();
            eb.er.pushRef(var, false);
            continue;
        }

        if (refs)
            escapeByRef(arg, &eb.er);
        else
            escapeByValue(arg, &eb.er);
    }

    void checkOnePair(size_t i, ref EscapeBy eb, ref EscapeBy eb2,
                      VarDeclaration v, VarDeclaration v2, bool of)
    {
        if (log) printf("v2: `%s`\n", v2.toChars());
        if (v2 != v)
            return;
        //printf("v %d v2 %d\n", eb.isMutable, eb2.isMutable);
        if (!(eb.isMutable || eb2.isMutable))
            return;

        if (!tf.islive && !(global.params.useDIP1000 == FeatureState.enabled && sc.func.setUnsafe()))
            return;

        if (!gag)
        {
            // int i; funcThatEscapes(ref int i);
            // funcThatEscapes(i); // error escaping reference _to_ `i`
            // int* j; funcThatEscapes2(int* j);
            // funcThatEscapes2(j); // error escaping reference _of_ `i`
            const(char)* referenceVerb = of ? "of" : "to";
            const(char)* msg = eb.isMutable && eb2.isMutable
                                ? "more than one mutable reference %s `%s` in arguments to `%s()`"
                                : "mutable and const references %s `%s` in arguments to `%s()`";
            error((*arguments)[i].loc, msg,
                  referenceVerb,
                  v.toChars(),
                  fd ? fd.toPrettyChars() : "indirectly");
        }
        errors = true;
    }

    void escape(size_t i, ref EscapeBy eb, bool byval)
    {
        foreach (VarDeclaration v; byval ? eb.er.byvalue : eb.er.byref)
        {
            if (log)
            {
                const(char)* by = byval ? "byval" : "byref";
                printf("%s %s\n", by, v.toChars());
            }
            if (byval && !v.type.hasPointers())
                continue;
            foreach (ref eb2; escapeBy[i + 1 .. $])
            {
                foreach (VarDeclaration v2; byval ? eb2.er.byvalue : eb2.er.byref)
                {
                    checkOnePair(i, eb, eb2, v, v2, byval);
                }
            }
        }
    }
    foreach (const i, ref eb; escapeBy[0 .. $ - 1])
    {
        escape(i, eb, true);
        escape(i, eb, false);
    }

    return errors;
}

/******************************************
 * Array literal is going to be allocated on the GC heap.
 * Check its elements to see if any would escape by going on the heap.
 * Params:
 *      sc = used to determine current function and module
 *      ae = array literal expression
 *      gag = do not print error messages
 * Returns:
 *      `true` if any elements escaped
 */
bool checkArrayLiteralEscape(Scope *sc, ArrayLiteralExp ae, bool gag)
{
    bool errors;
    if (ae.basis)
        errors = checkNewEscape(sc, ae.basis, gag);
    foreach (ex; *ae.elements)
    {
        if (ex)
            errors |= checkNewEscape(sc, ex, gag);
    }
    return errors;
}

/******************************************
 * Associative array literal is going to be allocated on the GC heap.
 * Check its elements to see if any would escape by going on the heap.
 * Params:
 *      sc = used to determine current function and module
 *      ae = associative array literal expression
 *      gag = do not print error messages
 * Returns:
 *      `true` if any elements escaped
 */
bool checkAssocArrayLiteralEscape(Scope *sc, AssocArrayLiteralExp ae, bool gag)
{
    bool errors;
    foreach (ex; *ae.keys)
    {
        if (ex)
            errors |= checkNewEscape(sc, ex, gag);
    }
    foreach (ex; *ae.values)
    {
        if (ex)
            errors |= checkNewEscape(sc, ex, gag);
    }
    return errors;
}

/**
 * A `scope` variable was assigned to non-scope parameter `v`.
 * If applicable, print why the parameter was not inferred `scope`.
 *
 * Params:
 *    printFunc = error/deprecation print function to use
 *    v = parameter that was not inferred
 *    recursionLimit = recursion limit for printing the reason
 */
void printScopeFailure(E)(E printFunc, VarDeclaration v, int recursionLimit)
{
    recursionLimit--;
    if (recursionLimit < 0 || !v)
        return;

    if (RootObject* o = v.sequenceNumber in EscapeState.scopeInferFailure)
    {
        switch ((*o).dyncast())
        {
            case DYNCAST.expression:
                Expression e = cast(Expression) *o;
                printFunc(e.loc, "which is not `scope` because of `%s`", e.toChars());
                break;
            case DYNCAST.dsymbol:
                VarDeclaration v1 = cast(VarDeclaration) *o;
                printFunc(v1.loc, "which is assigned to non-scope parameter `%s`", v1.toChars());
                printScopeFailure(printFunc, v1, recursionLimit);
                break;
            default:
                assert(0);
        }
    }
}

/****************************************
 * Function parameter `par` is being initialized to `arg`,
 * and `par` may escape.
 * Detect if scoped values can escape this way.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      fdc = function being called, `null` if called indirectly
 *      parId = name of function parameter for error messages
 *      vPar = `VarDeclaration` corresponding to `par`
 *      parStc = storage classes of function parameter (may have added `scope` from `pure`)
 *      arg = initializer for param
 *      assertmsg = true if the parameter is the msg argument to assert(bool, msg).
 *      gag = do not print error messages
 * Returns:
 *      `true` if pointers to the stack can escape via assignment
 */
bool checkParamArgumentEscape(Scope* sc, FuncDeclaration fdc, Identifier parId, VarDeclaration vPar, STC parStc, Expression arg, bool assertmsg, bool gag)
{
    enum log = false;
    if (log) printf("checkParamArgumentEscape(arg: %s par: %s)\n",
        arg ? arg.toChars() : "null",
        parId ? parId.toChars() : "null");
    //printf("type = %s, %d\n", arg.type.toChars(), arg.type.hasPointers());

    if (!arg.type.hasPointers())
        return false;

    EscapeByResults er;

    escapeByValue(arg, &er);

    if (parStc & STC.scope_)
    {
        // These errors only apply to non-scope parameters
        // When the paraneter is `scope`, only `checkScopeVarAddr` on `er.byref` is needed
        er.byfunc.setDim(0);
        er.byvalue.setDim(0);
        er.byexp.setDim(0);
    }

    if (!er.byref.length && !er.byvalue.length && !er.byfunc.length && !er.byexp.length)
        return false;

    bool result = false;

    /* 'v' is assigned unsafely to 'par'
     */
    void unsafeAssign(string desc)(VarDeclaration v)
    {
        if (assertmsg)
        {
            result |= sc.setUnsafeDIP1000(gag, arg.loc,
                desc ~ " `%s` assigned to non-scope parameter calling `assert()`", v);
            return;
        }
        const(char)* msg =
            (fdc &&  parId) ? (desc ~ " `%s` assigned to non-scope parameter `%s` calling `%s`") :
            (fdc && !parId) ? (desc ~ " `%s` assigned to non-scope anonymous parameter calling `%s`") :
            (!fdc && parId) ? (desc ~ " `%s` assigned to non-scope parameter `%s`") :
            (desc ~ " `%s` assigned to non-scope anonymous parameter");

        if (sc.setUnsafeDIP1000(gag, arg.loc, msg, v, parId ? parId : fdc, fdc))
        {
            result = true;
            printScopeFailure(previewSupplementalFunc(sc.isDeprecated(), global.params.useDIP1000), vPar, 10);
        }
    }

    foreach (VarDeclaration v; er.byvalue)
    {
        if (log) printf("byvalue %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        notMaybeScope(v, vPar);

        if (v.isScope())
        {
            unsafeAssign!"scope variable"(v);
        }
        else if (v.isTypesafeVariadicArray && p == sc.func)
        {
            unsafeAssign!"variadic variable"(v);
        }
    }

    foreach (VarDeclaration v; er.byref)
    {
        if (log) printf("byref %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        notMaybeScope(v, arg);
        if (checkScopeVarAddr(v, arg, sc, gag))
        {
            result = true;
            continue;
        }

        if (p == sc.func && !(parStc & STC.scope_))
        {
            unsafeAssign!"reference to local variable"(v);
            continue;
        }
    }

    foreach (FuncDeclaration fd; er.byfunc)
    {
        //printf("fd = %s, %d\n", fd.toChars(), fd.tookAddressOf);
        VarDeclarations vars;
        findAllOuterAccessedVariables(fd, &vars);

        foreach (v; vars)
        {
            //printf("v = %s\n", v.toChars());
            assert(!v.isDataseg());     // these are not put in the closureVars[]

            Dsymbol p = v.toParent2();

            notMaybeScope(v, arg);

            if ((v.isReference() || v.isScope()) && p == sc.func)
            {
                unsafeAssign!"reference to local"(v);
                continue;
            }
        }
    }

    if (!sc.func)
        return result;

    foreach (Expression ee; er.byexp)
    {
        const(char)* msg = parId ?
            "reference to stack allocated value returned by `%s` assigned to non-scope parameter `%s`" :
            "reference to stack allocated value returned by `%s` assigned to non-scope anonymous parameter";

        result |= sc.setUnsafeDIP1000(gag, ee.loc, msg, ee, parId);
    }

    return result;
}

/*****************************************************
 * Function argument initializes a `return` parameter,
 * and that parameter gets assigned to `firstArg`.
 * Essentially, treat as `firstArg = arg;`
 * Params:
 *      sc = used to determine current function and module
 *      firstArg = `ref` argument through which `arg` may be assigned
 *      arg = initializer for parameter
 *      param = parameter declaration corresponding to `arg`
 *      gag = do not print error messages
 * Returns:
 *      `true` if assignment to `firstArg` would cause an error
 */
bool checkParamArgumentReturn(Scope* sc, Expression firstArg, Expression arg, Parameter param, bool gag)
{
    enum log = false;
    if (log) printf("checkParamArgumentReturn(firstArg: %s arg: %s)\n",
        firstArg.toChars(), arg.toChars());
    //printf("type = %s, %d\n", arg.type.toChars(), arg.type.hasPointers());

    if (!(param.storageClass & STC.return_))
        return false;

    if (!arg.type.hasPointers() && !param.isReference())
        return false;

    // `byRef` needed for `assign(ref int* x, ref int i) {x = &i};`
    // Note: taking address of scope pointer is not allowed
    // `assign(ref int** x, return ref scope int* i) {x = &i};`
    // Thus no return ref/return scope ambiguity here
    const byRef = param.isReference() && !(param.storageClass & STC.scope_)
        && !(param.storageClass & STC.returnScope); // fixme: it's possible to infer returnScope without scope with vaIsFirstRef

    scope e = new AssignExp(arg.loc, firstArg, arg);
    return checkAssignEscape(sc, e, gag, byRef);
}

/*****************************************************
 * Check struct constructor of the form `s.this(args)`, by
 * checking each `return` parameter to see if it gets
 * assigned to `s`.
 * Params:
 *      sc = used to determine current function and module
 *      ce = constructor call of the form `s.this(args)`
 *      gag = do not print error messages
 * Returns:
 *      `true` if construction would cause an escaping reference error
 */
bool checkConstructorEscape(Scope* sc, CallExp ce, bool gag)
{
    enum log = false;
    if (log) printf("checkConstructorEscape(%s, %s)\n", ce.toChars(), ce.type.toChars());
    Type tthis = ce.type.toBasetype();
    assert(tthis.ty == Tstruct);
    if (!tthis.hasPointers())
        return false;

    if (!ce.arguments && ce.arguments.length)
        return false;

    DotVarExp dve = ce.e1.isDotVarExp();
    CtorDeclaration ctor = dve.var.isCtorDeclaration();
    TypeFunction tf = ctor.type.isTypeFunction();

    const nparams = tf.parameterList.length;
    const n = ce.arguments.length;

    // j=1 if _arguments[] is first argument
    const j = tf.isDstyleVariadic();

    /* Attempt to assign each `return` arg to the `this` reference
     */
    foreach (const i; 0 .. n)
    {
        Expression arg = (*ce.arguments)[i];
        //printf("\targ[%d]: %s\n", i, arg.toChars());

        if (i - j < nparams && i >= j)
        {
            Parameter p = tf.parameterList[i - j];
            if (checkParamArgumentReturn(sc, dve.e1, arg, p, gag))
                return true;
        }
    }

    return false;
}

/// How a `return` parameter escapes its pointer value
enum ReturnParamDest
{
    returnVal, /// through return statement: `return x`
    this_,     /// assigned to a struct instance: `this.x = x`
    firstArg,  /// assigned to first argument: `firstArg = x`
}

/****************************************
 * Find out if instead of returning a `return` parameter via a return statement,
 * it is returned via assignment to either `this` or the first parameter.
 *
 * This works the same as returning the value via a return statement.
 * Although the first argument must be `ref`, it is not regarded as returning by `ref`.
 *
 * See_Also: https://dlang.org.spec/function.html#return-ref-parameters
 *
 * Params:
 *   tf = function type
 *   tthis = type of `this` parameter, or `null` if none
 * Returns: What a `return` parameter should transfer the lifetime of the argument to
 */
ReturnParamDest returnParamDest(TypeFunction tf, Type tthis)
{
    assert(tf);
    if (tf.isctor)
        return ReturnParamDest.this_;

    if (!tf.nextOf() || (tf.nextOf().ty != Tvoid))
        return ReturnParamDest.returnVal;

    if (tthis && tthis.toBasetype().ty == Tstruct) // class `this` is passed by value
        return ReturnParamDest.this_;

    if (tf.parameterList.length > 0 && tf.parameterList[0].isReference)
        return ReturnParamDest.firstArg;

    return ReturnParamDest.returnVal;
}

/****************************************
 * Given an `AssignExp`, determine if the lvalue will cause
 * the contents of the rvalue to escape.
 * Print error messages when these are detected.
 * Infer `scope` attribute for the lvalue where possible, in order
 * to eliminate the error.
 * Params:
 *      sc = used to determine current function and module
 *      e = `AssignExp` or `CatAssignExp` to check for any pointers to the stack
 *      gag = do not print error messages
 *      byRef = set to `true` if `e1` of `e` gets assigned a reference to `e2`
 * Returns:
 *      `true` if pointers to the stack can escape via assignment
 */
bool checkAssignEscape(Scope* sc, Expression e, bool gag, bool byRef)
{
    enum log = false;
    if (log) printf("checkAssignEscape(e: %s, byRef: %d)\n", e.toChars(), byRef);
    if (e.op != EXP.assign && e.op != EXP.blit && e.op != EXP.construct &&
        e.op != EXP.concatenateAssign && e.op != EXP.concatenateElemAssign && e.op != EXP.concatenateDcharAssign)
        return false;
    auto ae = cast(BinExp)e;
    Expression e1 = ae.e1;
    Expression e2 = ae.e2;
    //printf("type = %s, %d\n", e1.type.toChars(), e1.type.hasPointers());

    if (!e1.type.hasPointers())
        return false;

    if (e1.isSliceExp())
    {
        if (VarDeclaration va = expToVariable(e1))
        {
            if (!va.type.toBasetype().isTypeSArray() || // treat static array slice same as a variable
                !va.type.hasPointers())
                return false;
        }
        else
            return false;
    }

    /* The struct literal case can arise from the S(e2) constructor call:
     *    return S(e2);
     * and appears in this function as:
     *    structLiteral = e2;
     * Such an assignment does not necessarily remove scope-ness.
     */
    if (e1.isStructLiteralExp())
        return false;

    VarDeclaration va = expToVariable(e1);
    EscapeByResults er;

    if (byRef)
        escapeByRef(e2, &er);
    else
        escapeByValue(e2, &er);

    if (!er.byref.length && !er.byvalue.length && !er.byfunc.length && !er.byexp.length)
        return false;


    if (va && e.op == EXP.concatenateElemAssign)
    {
        /* https://issues.dlang.org/show_bug.cgi?id=17842
         * Draw an equivalence between:
         *   *q = p;
         * and:
         *   va ~= e;
         * since we are not assigning to va, but are assigning indirectly through va.
         */
        va = null;
    }

    if (va && e1.isDotVarExp() && va.type.toBasetype().isTypeClass())
    {
        /* https://issues.dlang.org/show_bug.cgi?id=17949
         * Draw an equivalence between:
         *   *q = p;
         * and:
         *   va.field = e2;
         * since we are not assigning to va, but are assigning indirectly through class reference va.
         */
        va = null;
    }

    if (log && va) printf("va: %s\n", va.toChars());

    FuncDeclaration fd = sc.func;


    // Determine if va is a `ref` parameter, so it has a lifetime exceding the function scope
    const bool vaIsRef = va && va.isParameter() && va.isReference();
    if (log && vaIsRef) printf("va is ref `%s`\n", va.toChars());

    // Determine if va is the first parameter, through which other 'return' parameters
    // can be assigned.
    bool vaIsFirstRef = false;
    if (fd && fd.type)
    {
        final switch (returnParamDest(fd.type.isTypeFunction(), fd.vthis ? fd.vthis.type : null))
        {
            case ReturnParamDest.this_:
                vaIsFirstRef = va == fd.vthis;
                break;
            case ReturnParamDest.firstArg:
                vaIsFirstRef = (*fd.parameters)[0] == va;
                break;
            case ReturnParamDest.returnVal:
                break;
        }
    }
    if (log && vaIsFirstRef) printf("va is first ref `%s`\n", va.toChars());

    bool result = false;
    foreach (VarDeclaration v; er.byvalue)
    {
        if (log) printf("byvalue: %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        if (v == va)
            continue;

        Dsymbol p = v.toParent2();

        if (va && !vaIsRef && !va.isScope() && !v.isScope() &&
            !v.isTypesafeVariadicArray && !va.isTypesafeVariadicArray &&
            (va.isParameter() && va.maybeScope && v.isParameter() && v.maybeScope) &&
            p == fd)
        {
            /* Add v to va's list of dependencies
             */
            va.addMaybe(v);
            continue;
        }

        if (vaIsFirstRef && p == fd)
        {
            inferReturn(fd, v, /*returnScope:*/ true);
        }

        if (!(va && va.isScope()) || vaIsRef)
            notMaybeScope(v, e);

        if (v.isScope())
        {
            if (vaIsFirstRef && v.isParameter() && v.isReturn())
            {
                // va=v, where v is `return scope`
                if (inferScope(va))
                    continue;
            }

            // If va's lifetime encloses v's, then error
            if (EnclosedBy eb = va.enclosesLifetimeOf(v))
            {
                const(char)* msg;
                final switch (eb)
                {
                    case EnclosedBy.none: assert(0);
                    case EnclosedBy.returnScope:
                        msg = "scope variable `%s` assigned to return scope `%s`";
                        break;
                    case EnclosedBy.longerScope:
                        if (v.storage_class & STC.temp)
                            continue;
                        msg = "scope variable `%s` assigned to `%s` with longer lifetime";
                        break;
                    case EnclosedBy.refVar:
                        msg = "scope variable `%s` assigned to `ref` variable `%s` with longer lifetime";
                        break;
                    case EnclosedBy.global:
                        msg = "scope variable `%s` assigned to global variable `%s`";
                        break;
                }

                if (sc.setUnsafeDIP1000(gag, ae.loc, msg, v, va))
                {
                    result = true;
                    continue;
                }
            }

            // v = scope, va should be scope as well
            const vaWasScope = va && va.isScope();
            if (inferScope(va))
            {
                // In case of `scope local = returnScopeParam`, do not infer return scope for `x`
                if (!vaWasScope && v.isReturn() && !va.isReturn())
                {
                    if (log) printf("infer return for %s\n", va.toChars());
                    va.storage_class |= STC.return_ | STC.returninferred;

                    // Added "return scope" so don't confuse it with "return ref"
                    if (isRefReturnScope(va.storage_class))
                        va.storage_class |= STC.returnScope;
                }
                continue;
            }
            result |= sc.setUnsafeDIP1000(gag, ae.loc, "scope variable `%s` assigned to non-scope `%s`", v, e1);
        }
        else if (v.isTypesafeVariadicArray && p == fd)
        {
            if (inferScope(va))
                continue;
            result |= sc.setUnsafeDIP1000(gag, ae.loc, "variadic variable `%s` assigned to non-scope `%s`", v, e1);
        }
        else
        {
            /* v is not 'scope', and we didn't check the scope of where we assigned it to.
             * It may escape via that assignment, therefore, v can never be 'scope'.
             */
            //printf("no infer for %s in %s, %d\n", v.toChars(), fd.ident.toChars(), __LINE__);
            doNotInferScope(v, e);
        }
    }

    foreach (VarDeclaration v; er.byref)
    {
        if (log) printf("byref: %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        if (checkScopeVarAddr(v, ae, sc, gag))
        {
            result = true;
            continue;
        }

        if (va && va.isScope() && !v.isReference())
        {
            if (!va.isReturn())
            {
                va.doNotInferReturn = true;
            }
            else
            {
                result |= sc.setUnsafeDIP1000(gag, ae.loc,
                    "address of local variable `%s` assigned to return scope `%s`", v, va);
            }
        }

        Dsymbol p = v.toParent2();

        if (vaIsFirstRef && p == fd)
        {
            //if (log) printf("inferring 'return' for parameter %s in function %s\n", v.toChars(), fd.toChars());
            inferReturn(fd, v, /*returnScope:*/ false);
        }

        // If va's lifetime encloses v's, then error
        if (va && !(vaIsFirstRef && v.isReturn()) && va.enclosesLifetimeOf(v))
        {
            if (sc.setUnsafeDIP1000(gag, ae.loc, "address of variable `%s` assigned to `%s` with longer lifetime", v, va))
            {
                result = true;
                continue;
            }
        }

        if (!(va && va.isScope()))
            notMaybeScope(v, e);

        if (p != sc.func)
            continue;

        if (inferScope(va))
        {
            if (v.isReturn() && !va.isReturn())
                va.storage_class |= STC.return_ | STC.returninferred;
            continue;
        }
        if (e1.op == EXP.structLiteral)
            continue;

        result |= sc.setUnsafeDIP1000(gag, ae.loc, "reference to local variable `%s` assigned to non-scope `%s`", v, e1);
    }

    foreach (FuncDeclaration func; er.byfunc)
    {
        if (log) printf("byfunc: %s, %d\n", func.toChars(), func.tookAddressOf);
        VarDeclarations vars;
        findAllOuterAccessedVariables(func, &vars);

        /* https://issues.dlang.org/show_bug.cgi?id=16037
         * If assigning the address of a delegate to a scope variable,
         * then uncount that address of. This is so it won't cause a
         * closure to be allocated.
         */
        if (va && va.isScope() && !va.isReturn() && func.tookAddressOf)
            --func.tookAddressOf;

        foreach (v; vars)
        {
            //printf("v = %s\n", v.toChars());
            assert(!v.isDataseg());     // these are not put in the closureVars[]

            Dsymbol p = v.toParent2();

            if (!(va && va.isScope()))
                notMaybeScope(v, e);

            if (!(v.isReference() || v.isScope()) || p != fd)
                continue;

            if (va && !va.isDataseg() && (va.isScope() || va.maybeScope))
            {
                /* Don't infer STC.scope_ for va, because then a closure
                 * won't be generated for fd.
                 */
                //if (!va.isScope())
                    //va.storage_class |= STC.scope_ | STC.scopeinferred;
                continue;
            }
            result |= sc.setUnsafeDIP1000(gag, ae.loc,
                "reference to local `%s` assigned to non-scope `%s` in @safe code", v, e1);
        }
    }

    foreach (Expression ee; er.byexp)
    {
        if (log) printf("byexp: %s\n", ee.toChars());

        /* Do not allow slicing of a static array returned by a function
         */
        if (ee.op == EXP.call && ee.type.toBasetype().isTypeSArray() && e1.type.toBasetype().isTypeDArray() &&
            !(va && va.storage_class & STC.temp))
        {
            if (!gag)
                deprecation(ee.loc, "slice of static array temporary returned by `%s` assigned to longer lived variable `%s`",
                    ee.toChars(), e1.toChars());
            //result = true;
            continue;
        }

        if (ee.op == EXP.call && ee.type.toBasetype().isTypeStruct() &&
            (!va || !(va.storage_class & STC.temp) && !va.isScope()))
        {
            if (sc.setUnsafeDIP1000(gag, ee.loc, "address of struct temporary returned by `%s` assigned to longer lived variable `%s`", ee, e1))
            {
                result = true;
                continue;
            }
        }

        if (ee.op == EXP.structLiteral &&
            (!va || !(va.storage_class & STC.temp)))
        {
            if (sc.setUnsafeDIP1000(gag, ee.loc, "address of struct literal `%s` assigned to longer lived variable `%s`", ee, e1))
            {
                result = true;
                continue;
            }
        }

        if (inferScope(va))
            continue;

        result |= sc.setUnsafeDIP1000(gag, ee.loc,
            "reference to stack allocated value returned by `%s` assigned to non-scope `%s`", ee, e1);
    }

    return result;
}

/************************************
 * Detect cases where pointers to the stack can escape the
 * lifetime of the stack frame when throwing `e`.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check for any pointers to the stack
 *      gag = do not print error messages
 * Returns:
 *      `true` if pointers to the stack can escape
 */
bool checkThrowEscape(Scope* sc, Expression e, bool gag)
{
    //printf("[%s] checkThrowEscape, e = %s\n", e.loc.toChars(), e.toChars());
    EscapeByResults er;

    escapeByValue(e, &er);

    if (!er.byref.length && !er.byvalue.length && !er.byexp.length)
        return false;

    bool result = false;
    foreach (VarDeclaration v; er.byvalue)
    {
        //printf("byvalue %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        if (v.isScope() && !v.iscatchvar)       // special case: allow catch var to be rethrown
                                                // despite being `scope`
        {
            // https://issues.dlang.org/show_bug.cgi?id=17029
            result |= sc.setUnsafeDIP1000(gag, e.loc, "scope variable `%s` may not be thrown", v);
            continue;
        }
        else
        {
            notMaybeScope(v, new ThrowExp(e.loc, e));
        }
    }
    return result;
}

/************************************
 * Detect cases where pointers to the stack can escape the
 * lifetime of the stack frame by being placed into a GC allocated object.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check for any pointers to the stack
 *      gag = do not print error messages
 * Returns:
 *      `true` if pointers to the stack can escape
 */
bool checkNewEscape(Scope* sc, Expression e, bool gag)
{
    import dmd.globals: FeatureState;
    import dmd.errors: previewErrorFunc;

    //printf("[%s] checkNewEscape, e = %s\n", e.loc.toChars(), e.toChars());
    enum log = false;
    if (log) printf("[%s] checkNewEscape, e: `%s`\n", e.loc.toChars(), e.toChars());
    EscapeByResults er;

    escapeByValue(e, &er);

    if (!er.byref.length && !er.byvalue.length && !er.byexp.length)
        return false;

    bool result = false;
    foreach (VarDeclaration v; er.byvalue)
    {
        if (log) printf("byvalue `%s`\n", v.toChars());
        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        if (v.isScope())
        {
            if (
                /* This case comes up when the ReturnStatement of a __foreachbody is
                 * checked for escapes by the caller of __foreachbody. Skip it.
                 *
                 * struct S { static int opApply(int delegate(S*) dg); }
                 * S* foo() {
                 *    foreach (S* s; S) // create __foreachbody for body of foreach
                 *        return s;     // s is inferred as 'scope' but incorrectly tested in foo()
                 *    return null; }
                 */
                !(p.parent == sc.func))
            {
                // https://issues.dlang.org/show_bug.cgi?id=20868
                result |= sc.setUnsafeDIP1000(gag, e.loc, "scope variable `%s` may not be copied into allocated memory", v);
                continue;
            }
        }
        else if (v.isTypesafeVariadicArray && p == sc.func)
        {
            result |= sc.setUnsafeDIP1000(gag, e.loc,
                "copying `%s` into allocated memory escapes a reference to variadic parameter `%s`", e, v);
        }
        else
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            notMaybeScope(v, e);
        }
    }

    foreach (VarDeclaration v; er.byref)
    {
        if (log) printf("byref `%s`\n", v.toChars());

        // 'featureState' tells us whether to emit an error or a deprecation,
        // depending on the flag passed to the CLI for DIP25 / DIP1000
        bool escapingRef(VarDeclaration v, FeatureState fs)
        {
            const(char)* msg = v.isParameter() ?
                "copying `%s` into allocated memory escapes a reference to parameter `%s`" :
                "copying `%s` into allocated memory escapes a reference to local variable `%s`";
            return sc.setUnsafePreview(fs, gag, e.loc, msg, e, v);
        }

        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        if (!v.isReference())
        {
            if (p == sc.func)
            {
                result |= escapingRef(v, global.params.useDIP1000);
                continue;
            }
        }

        /* Check for returning a ref variable by 'ref', but should be 'return ref'
         * Infer the addition of 'return', or set result to be the offending expression.
         */
        if (!v.isReference())
            continue;

        // https://dlang.org/spec/function.html#return-ref-parameters
        if (p == sc.func)
        {
            //printf("escaping reference to local ref variable %s\n", v.toChars());
            //printf("storage class = x%llx\n", v.storage_class);
            result |= escapingRef(v, global.params.useDIP25);
            continue;
        }
        // Don't need to be concerned if v's parent does not return a ref
        FuncDeclaration func = p.isFuncDeclaration();
        if (!func || !func.type)
            continue;
        if (auto tf = func.type.isTypeFunction())
        {
            if (!tf.isref)
                continue;

            const(char)* msg = "storing reference to outer local variable `%s` into allocated memory causes it to escape";
            if (!gag)
            {
                previewErrorFunc(sc.isDeprecated(), global.params.useDIP25)(e.loc, msg, v.toChars());
            }

            // If -preview=dip25 is used, the user wants an error
            // Otherwise, issue a deprecation
            result |= (global.params.useDIP25 == FeatureState.enabled);
        }
    }

    foreach (Expression ee; er.byexp)
    {
        if (log) printf("byexp %s\n", ee.toChars());
        if (!gag)
            error(ee.loc, "storing reference to stack allocated value returned by `%s` into allocated memory causes it to escape",
                  ee.toChars());
        result = true;
    }

    return result;
}


/************************************
 * Detect cases where pointers to the stack can escape the
 * lifetime of the stack frame by returning `e` by value.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check for any pointers to the stack
 *      gag = do not print error messages
 * Returns:
 *      `true` if pointers to the stack can escape
 */
bool checkReturnEscape(Scope* sc, Expression e, bool gag)
{
    //printf("[%s] checkReturnEscape, e: %s\n", e.loc.toChars(), e.toChars());
    return checkReturnEscapeImpl(sc, e, false, gag);
}

/************************************
 * Detect cases where returning `e` by `ref` can result in a reference to the stack
 * being returned.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check
 *      gag = do not print error messages
 * Returns:
 *      `true` if references to the stack can escape
 */
bool checkReturnEscapeRef(Scope* sc, Expression e, bool gag)
{
    version (none)
    {
        printf("[%s] checkReturnEscapeRef, e = %s\n", e.loc.toChars(), e.toChars());
        printf("current function %s\n", sc.func.toChars());
        printf("parent2 function %s\n", sc.func.toParent2().toChars());
    }

    return checkReturnEscapeImpl(sc, e, true, gag);
}

/***************************************
 * Implementation of checking for escapes in return expressions.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check
 *      refs = `true`: escape by value, `false`: escape by `ref`
 *      gag = do not print error messages
 * Returns:
 *      `true` if references to the stack can escape
 */
private bool checkReturnEscapeImpl(Scope* sc, Expression e, bool refs, bool gag)
{
    enum log = false;
    if (log) printf("[%s] checkReturnEscapeImpl, refs: %d e: `%s`\n", e.loc.toChars(), refs, e.toChars());
    EscapeByResults er;

    if (refs)
        escapeByRef(e, &er);
    else
        escapeByValue(e, &er);

    if (!er.byref.length && !er.byvalue.length && !er.byexp.length)
        return false;

    bool result = false;
    foreach (VarDeclaration v; er.byvalue)
    {
        if (log) printf("byvalue `%s`\n", v.toChars());
        if (v.isDataseg())
            continue;

        const vsr = buildScopeRef(v.storage_class);

        Dsymbol p = v.toParent2();

        if (p == sc.func && inferReturn(sc.func, v, /*returnScope:*/ true))
        {
            continue;
        }

        if (v.isScope())
        {
            /* If `return scope` applies to v.
             */
            if (vsr == ScopeRef.ReturnScope ||
                vsr == ScopeRef.Ref_ReturnScope)
            {
                continue;
            }

            auto pfunc = p.isFuncDeclaration();
            if (pfunc &&
                /* This case comes up when the ReturnStatement of a __foreachbody is
                 * checked for escapes by the caller of __foreachbody. Skip it.
                 *
                 * struct S { static int opApply(int delegate(S*) dg); }
                 * S* foo() {
                 *    foreach (S* s; S) // create __foreachbody for body of foreach
                 *        return s;     // s is inferred as 'scope' but incorrectly tested in foo()
                 *    return null; }
                 */
                !(!refs && p.parent == sc.func && pfunc.fes) &&
                /*
                 *  auto p(scope string s) {
                 *      string scfunc() { return s; }
                 *  }
                 */
                !(!refs && sc.func.isFuncDeclaration().getLevel(pfunc, sc.intypeof) > 0)
               )
            {
                if (v.isParameter() && !v.isReturn())
                {
                    // https://issues.dlang.org/show_bug.cgi?id=23191
                    if (!gag)
                    {
                        previewErrorFunc(sc.isDeprecated(), global.params.useDIP1000)(e.loc,
                            "scope parameter `%s` may not be returned", v.toChars()
                        );
                        result = true;
                        continue;
                    }
                }
                else
                {
                    // https://issues.dlang.org/show_bug.cgi?id=17029
                    result |= sc.setUnsafeDIP1000(gag, e.loc, "scope variable `%s` may not be returned", v);
                    continue;
                }
            }
        }
        else if (v.isTypesafeVariadicArray && p == sc.func)
        {
            if (!gag)
                error(e.loc, "returning `%s` escapes a reference to variadic parameter `%s`", e.toChars(), v.toChars());
            result = false;
        }
        else
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            doNotInferScope(v, e);
        }
    }

    foreach (i, VarDeclaration v; er.byref[])
    {
        if (log)
        {
            printf("byref `%s` %s\n", v.toChars(), toChars(buildScopeRef(v.storage_class)));
        }

        // 'featureState' tells us whether to emit an error or a deprecation,
        // depending on the flag passed to the CLI for DIP25
        void escapingRef(VarDeclaration v, FeatureState featureState)
        {
            const(char)* msg = v.isParameter() ?
                "returning `%s` escapes a reference to parameter `%s`" :
                "returning `%s` escapes a reference to local variable `%s`";

            if (v.isParameter() && v.isReference())
            {
                if (sc.setUnsafePreview(featureState, gag, e.loc, msg, e, v) ||
                    sc.func.isSafeBypassingInference())
                {
                    result = true;
                    if (v.storage_class & STC.returnScope)
                    {
                        previewSupplementalFunc(sc.isDeprecated(), featureState)(v.loc,
                            "perhaps change the `return scope` into `scope return`");
                    }
                    else
                    {
                        const(char)* annotateKind = (v.ident is Id.This) ? "function" : "parameter";
                        previewSupplementalFunc(sc.isDeprecated(), featureState)(v.loc,
                            "perhaps annotate the %s with `return`", annotateKind);
                    }
                }
            }
            else
            {
                if (er.refRetRefTransition[i])
                {
                    result |= sc.setUnsafeDIP1000(gag, e.loc, msg, e, v);
                }
                else
                {
                    if (!gag)
                        previewErrorFunc(sc.isDeprecated(), featureState)(e.loc, msg, e.toChars(), v.toChars());
                    result = true;
                }
            }
        }

        if (v.isDataseg())
            continue;

        const vsr = buildScopeRef(v.storage_class);

        Dsymbol p = v.toParent2();

        // https://issues.dlang.org/show_bug.cgi?id=19965
        if (!refs)
        {
            if (sc.func.vthis == v)
                notMaybeScope(v, e);

            if (checkScopeVarAddr(v, e, sc, gag))
            {
                result = true;
                continue;
            }
        }

        if (!v.isReference())
        {
            if (p == sc.func)
            {
                escapingRef(v, FeatureState.enabled);
                continue;
            }
            FuncDeclaration fd = p.isFuncDeclaration();
            if (fd && sc.func.returnInprocess)
            {
                /* Code like:
                 *   int x;
                 *   auto dg = () { return &x; }
                 * Making it:
                 *   auto dg = () return { return &x; }
                 * Because dg.ptr points to x, this is returning dt.ptr+offset
                 */
                sc.func.storage_class |= STC.return_ | STC.returninferred;
            }
        }

        /* Check for returning a ref variable by 'ref', but should be 'return ref'
         * Infer the addition of 'return', or set result to be the offending expression.
         */
        if ((vsr == ScopeRef.Ref ||
             vsr == ScopeRef.RefScope ||
             vsr == ScopeRef.Ref_ReturnScope) &&
            !(v.storage_class & STC.foreach_))
        {
            if (p == sc.func && (vsr == ScopeRef.Ref || vsr == ScopeRef.RefScope) &&
                inferReturn(sc.func, v, /*returnScope:*/ false))
            {
                continue;
            }
            else
            {
                // https://dlang.org/spec/function.html#return-ref-parameters
                // Only look for errors if in module listed on command line
                if (p == sc.func)
                {
                    //printf("escaping reference to local ref variable %s\n", v.toChars());
                    //printf("storage class = x%llx\n", v.storage_class);
                    escapingRef(v, global.params.useDIP25);
                    continue;
                }
                // Don't need to be concerned if v's parent does not return a ref
                FuncDeclaration fd = p.isFuncDeclaration();
                if (fd && fd.type && fd.type.ty == Tfunction)
                {
                    TypeFunction tf = fd.type.isTypeFunction();
                    if (tf.isref)
                    {
                        const(char)* msg = "escaping reference to outer local variable `%s`";
                        if (!gag)
                            previewErrorFunc(sc.isDeprecated(), global.params.useDIP25)(e.loc, msg, v.toChars());
                        result = true;
                        continue;
                    }
                }

            }
        }
    }

    foreach (i, Expression ee; er.byexp[])
    {
        if (log) printf("byexp %s\n", ee.toChars());
        if (er.expRetRefTransition[i])
        {
            result |= sc.setUnsafeDIP1000(gag, ee.loc,
                "escaping reference to stack allocated value returned by `%s`", ee);
        }
        else
        {
            if (!gag)
                error(ee.loc, "escaping reference to stack allocated value returned by `%s`", ee.toChars());
            result = true;
        }
    }
    return result;
}

/***********************************
 * Infer `scope` for a variable
 *
 * Params:
 *      va = variable to infer scope for
 * Returns: `true` if succesful or already `scope`
 */
bool inferScope(VarDeclaration va)
{
    if (!va)
        return false;
    if (!va.isDataseg() && va.maybeScope && !va.isScope())
    {
        //printf("inferring scope for %s\n", va.toChars());
        va.maybeScope = false;
        va.storage_class |= STC.scope_ | STC.scopeinferred;
        return true;
    }
    return va.isScope();
}

/*************************************
 * Variable v needs to have 'return' inferred for it.
 * Params:
 *      fd = function that v is a parameter to
 *      v = parameter that needs to be STC.return_
 *      returnScope = infer `return scope` instead of `return ref`
 *
 * Returns: whether the inference on `v` was successful or `v` already was `return`
 */
private bool inferReturn(FuncDeclaration fd, VarDeclaration v, bool returnScope)
{
    if (v.isReturn())
        return !!(v.storage_class & STC.returnScope) == returnScope;

    if (!v.isParameter() || v.isTypesafeVariadicArray || (returnScope && v.doNotInferReturn))
        return false;

    if (!fd.returnInprocess)
        return false;

    if (returnScope && !(v.isScope() || v.maybeScope))
        return false;

    //printf("for function '%s' inferring 'return' for variable '%s', returnScope: %d\n", fd.toChars(), v.toChars(), returnScope);
    auto newStcs = STC.return_ | STC.returninferred | (returnScope ? STC.returnScope : 0);
    v.storage_class |= newStcs;

    if (v == fd.vthis)
    {
        /* v is the 'this' reference, so mark the function
         */
        fd.storage_class |= newStcs;
        if (auto tf = fd.type.isTypeFunction())
        {
            //printf("'this' too %p %s\n", tf, sc.func.toChars());
            tf.isreturnscope = returnScope;
            tf.isreturn = true;
            tf.isreturninferred = true;
        }
    }
    else
    {
        // Perform 'return' inference on parameter
        if (auto tf = fd.type.isTypeFunction())
        {
            foreach (i, p; tf.parameterList)
            {
                if (p.ident == v.ident)
                {
                    p.storageClass |= newStcs;
                    break;              // there can be only one
                }
            }
        }
    }
    return true;
}


/****************************************
 * e is an expression to be returned by value, and that value contains pointers.
 * Walk e to determine which variables are possibly being
 * returned by value, such as:
 *      int* function(int* p) { return p; }
 * If e is a form of &p, determine which variables have content
 * which is being returned as ref, such as:
 *      int* function(int i) { return &i; }
 * Multiple variables can be inserted, because of expressions like this:
 *      int function(bool b, int i, int* p) { return b ? &i : p; }
 *
 * No side effects.
 *
 * Params:
 *      e = expression to be returned by value
 *      er = where to place collected data
 *      live = if @live semantics apply, i.e. expressions `p`, `*p`, `**p`, etc., all return `p`.
  *     retRefTransition = if `e` is returned through a `return ref scope` function call
 */
void escapeByValue(Expression e, EscapeByResults* er, bool live = false, bool retRefTransition = false)
{
    //printf("[%s] escapeByValue, e: %s\n", e.loc.toChars(), e.toChars());

    void visit(Expression e)
    {
    }

    void visitAddr(AddrExp e)
    {
        /* Taking the address of struct literal is normally not
         * allowed, but CTFE can generate one out of a new expression,
         * but it'll be placed in static data so no need to check it.
         */
        if (e.e1.op != EXP.structLiteral)
            escapeByRef(e.e1, er, live, retRefTransition);
    }

    void visitSymOff(SymOffExp e)
    {
        VarDeclaration v = e.var.isVarDeclaration();
        if (v)
            er.pushRef(v, retRefTransition);
    }

    void visitVar(VarExp e)
    {
        if (auto v = e.var.isVarDeclaration())
        {
            if (v.type.hasPointers() || // not tracking non-pointers
                v.storage_class & STC.lazy_) // lazy variables are actually pointers
                er.byvalue.push(v);
        }
    }

    void visitThis(ThisExp e)
    {
        if (e.var)
            er.byvalue.push(e.var);
    }

    void visitPtr(PtrExp e)
    {
        if (live && e.type.hasPointers())
            escapeByValue(e.e1, er, live, retRefTransition);
    }

    void visitDotVar(DotVarExp e)
    {
        auto t = e.e1.type.toBasetype();
        if (e.type.hasPointers() && (live || t.ty == Tstruct))
        {
            escapeByValue(e.e1, er, live, retRefTransition);
        }
    }

    void visitDelegate(DelegateExp e)
    {
        Type t = e.e1.type.toBasetype();
        if (t.ty == Tclass || t.ty == Tpointer)
            escapeByValue(e.e1, er, live, retRefTransition);
        else
            escapeByRef(e.e1, er, live, retRefTransition);
        er.byfunc.push(e.func);
    }

    void visitFunc(FuncExp e)
    {
        if (e.fd.tok == TOK.delegate_)
            er.byfunc.push(e.fd);
    }

    void visitTuple(TupleExp e)
    {
        assert(0); // should have been lowered by now
    }

    void visitArrayLiteral(ArrayLiteralExp e)
    {
        Type tb = e.type.toBasetype();
        if (tb.ty == Tsarray || tb.ty == Tarray)
        {
            if (e.basis)
                escapeByValue(e.basis, er, live, retRefTransition);
            foreach (el; *e.elements)
            {
                if (el)
                    escapeByValue(el, er, live, retRefTransition);
            }
        }
    }

    void visitStructLiteral(StructLiteralExp e)
    {
        if (e.elements)
        {
            foreach (ex; *e.elements)
            {
                if (ex)
                    escapeByValue(ex, er, live, retRefTransition);
            }
        }
    }

    void visitNew(NewExp e)
    {
        Type tb = e.newtype.toBasetype();
        if (tb.ty == Tstruct && !e.member && e.arguments)
        {
            foreach (ex; *e.arguments)
            {
                if (ex)
                    escapeByValue(ex, er, live, retRefTransition);
            }
        }
    }

    void visitCast(CastExp e)
    {
        if (!e.type.hasPointers())
            return;
        Type tb = e.type.toBasetype();
        if (tb.ty == Tarray && e.e1.type.toBasetype().ty == Tsarray)
        {
            escapeByRef(e.e1, er, live, retRefTransition);
        }
        else
            escapeByValue(e.e1, er, live, retRefTransition);
    }

    void visitSlice(SliceExp e)
    {
        if (auto ve = e.e1.isVarExp())
        {
            VarDeclaration v = ve.var.isVarDeclaration();
            Type tb = e.type.toBasetype();
            if (v)
            {
                if (tb.ty == Tsarray)
                    return;
                if (v.isTypesafeVariadicArray)
                {
                    er.byvalue.push(v);
                    return;
                }
            }
        }
        Type t1b = e.e1.type.toBasetype();
        if (t1b.ty == Tsarray)
        {
            Type tb = e.type.toBasetype();
            if (tb.ty != Tsarray)
                escapeByRef(e.e1, er, live, retRefTransition);
        }
        else
            escapeByValue(e.e1, er, live, retRefTransition);
    }

    void visitIndex(IndexExp e)
    {
        if (e.e1.type.toBasetype().ty == Tsarray ||
            live && e.type.hasPointers())
        {
            escapeByValue(e.e1, er, live, retRefTransition);
        }
    }

    void visitBin(BinExp e)
    {
        Type tb = e.type.toBasetype();
        if (tb.ty == Tpointer)
        {
            escapeByValue(e.e1, er, live, retRefTransition);
            escapeByValue(e.e2, er, live, retRefTransition);
        }
    }

    void visitBinAssign(BinAssignExp e)
    {
        escapeByValue(e.e1, er, live, retRefTransition);
    }

    void visitAssign(AssignExp e)
    {
        escapeByValue(e.e1, er, live, retRefTransition);
    }

    void visitComma(CommaExp e)
    {
        escapeByValue(e.e2, er, live, retRefTransition);
    }

    void visitCond(CondExp e)
    {
        escapeByValue(e.e1, er, live, retRefTransition);
        escapeByValue(e.e2, er, live, retRefTransition);
    }

    void visitCall(CallExp e)
    {
        //printf("CallExp(): %s\n", e.toChars());
        /* Check each argument that is
         * passed as 'return scope'.
         */
        Type t1 = e.e1.type.toBasetype();
        TypeFunction tf;
        TypeDelegate dg;
        if (t1.ty == Tdelegate)
        {
            dg = t1.isTypeDelegate();
            tf = dg.next.isTypeFunction();
        }
        else if (t1.ty == Tfunction)
            tf = t1.isTypeFunction();
        else
            return;

        if (!e.type.hasPointers())
            return;

        if (e.arguments && e.arguments.length)
        {
            /* j=1 if _arguments[] is first argument,
             * skip it because it is not passed by ref
             */
            int j = tf.isDstyleVariadic();
            for (size_t i = j; i < e.arguments.length; ++i)
            {
                Expression arg = (*e.arguments)[i];
                size_t nparams = tf.parameterList.length;
                if (i - j < nparams && i >= j)
                {
                    Parameter p = tf.parameterList[i - j];
                    const stc = tf.parameterStorageClass(null, p);
                    ScopeRef psr = buildScopeRef(stc);
                    if (psr == ScopeRef.ReturnScope || psr == ScopeRef.Ref_ReturnScope)
                    {
                        if (tf.isref)
                        {
                            /* ignore `ref` on struct constructor return because
                             *   struct S { this(return scope int* q) { this.p = q; } int* p; }
                             * is different from:
                             *   ref char* front(return scope char** q) { return *q; }
                             * https://github.com/dlang/dmd/pull/14869
                             */
                            if (auto dve = e.e1.isDotVarExp())
                                if (auto fd = dve.var.isFuncDeclaration())
                                    if (fd.isCtorDeclaration() && tf.next.toBasetype().isTypeStruct())
                                    {
                                        escapeByValue(arg, er, live, retRefTransition);
                                    }
                        }
                        else
                            escapeByValue(arg, er, live, retRefTransition);
                    }
                    else if (psr == ScopeRef.ReturnRef || psr == ScopeRef.ReturnRef_Scope)
                    {
                        if (tf.isref)
                        {
                            /* Treat:
                             *   ref P foo(return ref P p)
                             * as:
                             *   p;
                             */
                            escapeByValue(arg, er, live, retRefTransition);
                        }
                        else
                            escapeByRef(arg, er, live, retRefTransition);
                    }
                }
            }
        }
        // If 'this' is returned, check it too
        if (e.e1.op == EXP.dotVariable && t1.ty == Tfunction)
        {
            DotVarExp dve = e.e1.isDotVarExp();
            FuncDeclaration fd = dve.var.isFuncDeclaration();
            if (fd && fd.isThis())
            {
                /* Calling a non-static member function dve.var, which is returning `this`, and with dve.e1 representing `this`
                 */

                /*****************************
                 * Concoct storage class for member function's implicit `this` parameter.
                 * Params:
                 *      fd = member function
                 * Returns:
                 *      storage class for fd's `this`
                 */
                StorageClass getThisStorageClass(FuncDeclaration fd)
                {
                    StorageClass stc;
                    auto tf = fd.type.toBasetype().isTypeFunction();
                    if (tf.isreturn)
                        stc |= STC.return_;
                    if (tf.isreturnscope)
                        stc |= STC.returnScope | STC.scope_;
                    auto ad = fd.isThis();
                    if (ad.isClassDeclaration() || tf.isScopeQual)
                        stc |= STC.scope_;
                    if (ad.isStructDeclaration())
                        stc |= STC.ref_;        // `this` for a struct member function is passed by `ref`
                    return stc;
                }

                const psr = buildScopeRef(getThisStorageClass(fd));
                if (psr == ScopeRef.ReturnScope || psr == ScopeRef.Ref_ReturnScope)
                {
                    if (!tf.isref || tf.isctor)
                        escapeByValue(dve.e1, er, live, retRefTransition);
                }
                else if (psr == ScopeRef.ReturnRef || psr == ScopeRef.ReturnRef_Scope)
                {
                    if (tf.isref)
                    {
                        /* Treat calling:
                         *   struct S { ref S foo() return; }
                         * as:
                         *   this;
                         */
                        escapeByValue(dve.e1, er, live, retRefTransition);
                    }
                    else
                        escapeByRef(dve.e1, er, live, psr == ScopeRef.ReturnRef_Scope);
                }
            }

            // If it's also a nested function that is 'return scope'
            if (fd && fd.isNested())
            {
                if (tf.isreturn && tf.isScopeQual)
                    er.pushExp(e, false);
            }
        }

        /* If returning the result of a delegate call, the .ptr
         * field of the delegate must be checked.
         */
        if (dg)
        {
            if (tf.isreturn)
                escapeByValue(e.e1, er, live, retRefTransition);
        }

        /* If it's a nested function that is 'return scope'
         */
        if (auto ve = e.e1.isVarExp())
        {
            FuncDeclaration fd = ve.var.isFuncDeclaration();
            if (fd && fd.isNested())
            {
                if (tf.isreturn && tf.isScopeQual)
                    er.pushExp(e, false);
            }
        }
    }

    switch (e.op)
    {
        case EXP.address: return visitAddr(e.isAddrExp());
        case EXP.symbolOffset: return visitSymOff(e.isSymOffExp());
        case EXP.variable: return visitVar(e.isVarExp());
        case EXP.this_: return visitThis(e.isThisExp());
        case EXP.star: return visitPtr(e.isPtrExp());
        case EXP.dotVariable: return visitDotVar(e.isDotVarExp());
        case EXP.delegate_: return visitDelegate(e.isDelegateExp());
        case EXP.function_: return visitFunc(e.isFuncExp());
        case EXP.tuple: return visitTuple(e.isTupleExp());
        case EXP.arrayLiteral: return visitArrayLiteral(e.isArrayLiteralExp());
        case EXP.structLiteral: return visitStructLiteral(e.isStructLiteralExp());
        case EXP.new_: return visitNew(e.isNewExp());
        case EXP.cast_: return visitCast(e.isCastExp());
        case EXP.slice: return visitSlice(e.isSliceExp());
        case EXP.index: return visitIndex(e.isIndexExp());
        case EXP.blit: return visitAssign(e.isBlitExp());
        case EXP.construct: return visitAssign(e.isConstructExp());
        case EXP.assign: return visitAssign(e.isAssignExp());
        case EXP.comma: return visitComma(e.isCommaExp());
        case EXP.question: return visitCond(e.isCondExp());
        case EXP.call: return visitCall(e.isCallExp());
        default:
            if (auto b = e.isBinExp())
                return visitBin(b);
            if (auto ba = e.isBinAssignExp())
                return visitBinAssign(ba);
            return visit(e);
    }
}


/****************************************
 * e is an expression to be returned by 'ref'.
 * Walk e to determine which variables are possibly being
 * returned by ref, such as:
 *      ref int function(int i) { return i; }
 * If e is a form of *p, determine which variables have content
 * which is being returned as ref, such as:
 *      ref int function(int* p) { return *p; }
 * Multiple variables can be inserted, because of expressions like this:
 *      ref int function(bool b, int i, int* p) { return b ? i : *p; }
 *
 * No side effects.
 *
 * Params:
 *      e = expression to be returned by 'ref'
 *      er = where to place collected data
 *      live = if @live semantics apply, i.e. expressions `p`, `*p`, `**p`, etc., all return `p`.
 *      retRefTransition = if `e` is returned through a `return ref scope` function call
 */
void escapeByRef(Expression e, EscapeByResults* er, bool live = false, bool retRefTransition = false)
{
    //printf("[%s] escapeByRef, e: %s, retRefTransition: %d\n", e.loc.toChars(), e.toChars(), retRefTransition);
    void visit(Expression e)
    {
    }

    void visitVar(VarExp e)
    {
        auto v = e.var.isVarDeclaration();
        if (v)
        {
            if (v.storage_class & STC.ref_ && v.storage_class & (STC.foreach_ | STC.temp) && v._init)
            {
                /* If compiler generated ref temporary
                    *   (ref v = ex; ex)
                    * look at the initializer instead
                    */
                if (ExpInitializer ez = v._init.isExpInitializer())
                {
                    if (auto ce = ez.exp.isConstructExp())
                        escapeByRef(ce.e2, er, live, retRefTransition);
                    else
                        escapeByRef(ez.exp, er, live, retRefTransition);
                }
            }
            else
                er.pushRef(v, retRefTransition);
        }
    }

    void visitThis(ThisExp e)
    {
        if (e.var && e.var.toParent2().isFuncDeclaration().hasDualContext())
            escapeByValue(e, er, live, retRefTransition);
        else if (e.var)
            er.pushRef(e.var, retRefTransition);
    }

    void visitPtr(PtrExp e)
    {
        escapeByValue(e.e1, er, live, retRefTransition);
    }

    void visitIndex(IndexExp e)
    {
        Type tb = e.e1.type.toBasetype();
        if (auto ve = e.e1.isVarExp())
        {
            VarDeclaration v = ve.var.isVarDeclaration();
            if (v && v.isTypesafeVariadicArray)
            {
                er.pushRef(v, retRefTransition);
                return;
            }
        }
        if (tb.ty == Tsarray)
        {
            escapeByRef(e.e1, er, live, retRefTransition);
        }
        else if (tb.ty == Tarray)
        {
            escapeByValue(e.e1, er, live, retRefTransition);
        }
    }

    void visitStructLiteral(StructLiteralExp e)
    {
        if (e.elements)
        {
            foreach (ex; *e.elements)
            {
                if (ex)
                    escapeByRef(ex, er, live, retRefTransition);
            }
        }
        er.pushExp(e, retRefTransition);
    }

    void visitDotVar(DotVarExp e)
    {
        Type t1b = e.e1.type.toBasetype();
        if (t1b.ty == Tclass)
            escapeByValue(e.e1, er, live, retRefTransition);
        else
            escapeByRef(e.e1, er, live, retRefTransition);
    }

    void visitBinAssign(BinAssignExp e)
    {
        escapeByRef(e.e1, er, live, retRefTransition);
    }

    void visitAssign(AssignExp e)
    {
        escapeByRef(e.e1, er, live, retRefTransition);
    }

    void visitComma(CommaExp e)
    {
        escapeByRef(e.e2, er, live, retRefTransition);
    }

    void visitCond(CondExp e)
    {
        escapeByRef(e.e1, er, live, retRefTransition);
        escapeByRef(e.e2, er, live, retRefTransition);
    }

    void visitCall(CallExp e)
    {
        //printf("escapeByRef.CallExp(): %s\n", e.toChars());
        /* If the function returns by ref, check each argument that is
         * passed as 'return ref'.
         */
        Type t1 = e.e1.type.toBasetype();
        TypeFunction tf;
        if (t1.ty == Tdelegate)
            tf = t1.isTypeDelegate().next.isTypeFunction();
        else if (t1.ty == Tfunction)
            tf = t1.isTypeFunction();
        else
            return;
        if (tf.isref)
        {
            if (e.arguments && e.arguments.length)
            {
                /* j=1 if _arguments[] is first argument,
                 * skip it because it is not passed by ref
                 */
                int j = tf.isDstyleVariadic();
                for (size_t i = j; i < e.arguments.length; ++i)
                {
                    Expression arg = (*e.arguments)[i];
                    size_t nparams = tf.parameterList.length;
                    if (i - j < nparams && i >= j)
                    {
                        Parameter p = tf.parameterList[i - j];
                        const stc = tf.parameterStorageClass(null, p);
                        ScopeRef psr = buildScopeRef(stc);
                        if (psr == ScopeRef.ReturnRef || psr == ScopeRef.ReturnRef_Scope)
                            escapeByRef(arg, er, live, retRefTransition);
                        else if (psr == ScopeRef.ReturnScope || psr == ScopeRef.Ref_ReturnScope)
                        {
                            if (auto de = arg.isDelegateExp())
                            {
                                if (de.func.isNested())
                                    er.pushExp(de, false);
                            }
                            else
                                escapeByValue(arg, er, live, retRefTransition);
                        }
                    }
                }
            }
            // If 'this' is returned by ref, check it too
            if (e.e1.op == EXP.dotVariable && t1.ty == Tfunction)
            {
                DotVarExp dve = e.e1.isDotVarExp();

                // https://issues.dlang.org/show_bug.cgi?id=20149#c10
                if (dve.var.isCtorDeclaration())
                {
                    er.pushExp(e, false);
                    return;
                }

                StorageClass stc = dve.var.storage_class & (STC.return_ | STC.scope_ | STC.ref_);
                if (tf.isreturn)
                    stc |= STC.return_;
                if (tf.isref)
                    stc |= STC.ref_;
                if (tf.isScopeQual)
                    stc |= STC.scope_;
                if (tf.isreturnscope)
                    stc |= STC.returnScope;

                const psr = buildScopeRef(stc);
                if (psr == ScopeRef.ReturnRef || psr == ScopeRef.ReturnRef_Scope)
                    escapeByRef(dve.e1, er, live, psr == ScopeRef.ReturnRef_Scope);
                else if (psr == ScopeRef.ReturnScope || psr == ScopeRef.Ref_ReturnScope)
                    escapeByValue(dve.e1, er, live, retRefTransition);

                // If it's also a nested function that is 'return ref'
                if (FuncDeclaration fd = dve.var.isFuncDeclaration())
                {
                    if (fd.isNested() && tf.isreturn)
                    {
                        er.pushExp(e, false);
                    }
                }
            }
            // If it's a delegate, check it too
            if (e.e1.op == EXP.variable && t1.ty == Tdelegate)
            {
                escapeByValue(e.e1, er, live, retRefTransition);
            }

            /* If it's a nested function that is 'return ref'
             */
            if (auto ve = e.e1.isVarExp())
            {
                FuncDeclaration fd = ve.var.isFuncDeclaration();
                if (fd && fd.isNested())
                {
                    if (tf.isreturn)
                        er.pushExp(e, false);
                }
            }
        }
        else
            er.pushExp(e, retRefTransition);
    }

    switch (e.op)
    {
        case EXP.variable: return visitVar(e.isVarExp());
        case EXP.this_: return visitThis(e.isThisExp());
        case EXP.star: return visitPtr(e.isPtrExp());
        case EXP.structLiteral: return visitStructLiteral(e.isStructLiteralExp());
        case EXP.dotVariable: return visitDotVar(e.isDotVarExp());
        case EXP.index: return visitIndex(e.isIndexExp());
        case EXP.blit: return visitAssign(e.isBlitExp());
        case EXP.construct: return visitAssign(e.isConstructExp());
        case EXP.assign: return visitAssign(e.isAssignExp());
        case EXP.comma: return visitComma(e.isCommaExp());
        case EXP.question: return visitCond(e.isCondExp());
        case EXP.call: return visitCall(e.isCallExp());
        default:
            if (auto ba = e.isBinAssignExp())
                return visitBinAssign(ba);
            return visit(e);
    }
}

/************************************
 * Aggregate the data collected by the escapeBy??() functions.
 */
struct EscapeByResults
{
    VarDeclarations byref;      // array into which variables being returned by ref are inserted
    VarDeclarations byvalue;    // array into which variables with values containing pointers are inserted
    private FuncDeclarations byfunc; // nested functions that are turned into delegates
    private Expressions byexp;       // array into which temporaries being returned by ref are inserted

    import dmd.root.array: Array;

    /**
     * Whether the variable / expression went through a `return ref scope` function call
     *
     * This is needed for the dip1000 by default transition, since the rules for
     * disambiguating `return scope ref` have changed. Therefore, functions in legacy code
     * can be mistakenly treated as `return ref` making the compiler believe stack variables
     * are being escaped, which is an error even in `@system` code. By keeping track of this
     * information, variables escaped through `return ref` can be treated as a deprecation instead
     * of error, see test/fail_compilation/dip1000_deprecation.d
     */
    private Array!bool refRetRefTransition;
    private Array!bool expRetRefTransition;

    /** Reset arrays so the storage can be used again
     */
    void reset()
    {
        byref.setDim(0);
        byvalue.setDim(0);
        byfunc.setDim(0);
        byexp.setDim(0);

        refRetRefTransition.setDim(0);
        expRetRefTransition.setDim(0);
    }

    /**
     * Escape variable `v` by reference
     * Params:
     *   v = variable to escape
     *   retRefTransition = `v` is escaped through a `return ref scope` function call
     */
    void pushRef(VarDeclaration v, bool retRefTransition)
    {
        byref.push(v);
        refRetRefTransition.push(retRefTransition);
    }

    /**
     * Escape a reference to expression `e`
     * Params:
     *   e = expression to escape
     *   retRefTransition = `e` is escaped through a `return ref scope` function call
     */
    void pushExp(Expression e, bool retRefTransition)
    {
        byexp.push(e);
        expRetRefTransition.push(retRefTransition);
    }
}

/*************************
 * Find all variables accessed by this delegate that are
 * in functions enclosing it.
 * Params:
 *      fd = function
 *      vars = array to append found variables to
 */
public void findAllOuterAccessedVariables(FuncDeclaration fd, VarDeclarations* vars)
{
    //printf("findAllOuterAccessedVariables(fd: %s)\n", fd.toChars());
    for (auto p = fd.parent; p; p = p.parent)
    {
        auto fdp = p.isFuncDeclaration();
        if (!fdp)
            continue;

        foreach (v; fdp.closureVars)
        {
            foreach (const fdv; v.nestedrefs)
            {
                if (fdv == fd)
                {
                    //printf("accessed: %s, type %s\n", v.toChars(), v.type.toChars());
                    vars.push(v);
                }
            }
        }
    }
}

/***********************************
 * Turn off `maybeScope` for variable `v`.
 *
 * This exists in order to find where `maybeScope` is getting turned off.
 * Params:
 *      v = variable
 *      o = reason for it being turned off:
 *          - `Expression` such as `throw e` or `&e`
 *          - `VarDeclaration` of a non-scope parameter it was assigned to
 *          - `null` for no reason
 */
private void notMaybeScope(VarDeclaration v, RootObject o)
{
    if (v.maybeScope)
    {
        v.maybeScope = false;
        if (o && v.isParameter())
            EscapeState.scopeInferFailure[v.sequenceNumber] = o;
    }
}

/***********************************
 * Turn off `maybeScope` for variable `v` if it's not a parameter.
 *
 * This is for compatibility with the old system with both `STC.maybescope` and `VarDeclaration.doNotInferScope`,
 * which is now just `VarDeclaration.maybeScope`.
 * This function should probably be removed in future refactors.
 *
 * Params:
 *      v = variable
 *      o = reason for it being turned off
 */
private void doNotInferScope(VarDeclaration v, RootObject o)
{
    if (!v.isParameter)
        notMaybeScope(v, o);
}

/***********************************
 * After semantic analysis of the function body,
 * try to infer `scope` / `return` on the parameters
 *
 * Params:
 *   funcdecl = function declaration that was analyzed
 *   f = final function type. `funcdecl.type` started as the 'premature type' before attribute
 *       inference, then its inferred attributes are copied over to final type `f`
 */
void finishScopeParamInference(FuncDeclaration funcdecl, ref TypeFunction f)
{

    if (funcdecl.returnInprocess)
    {
        funcdecl.returnInprocess = false;
        if (funcdecl.storage_class & STC.return_)
        {
            if (funcdecl.type == f)
                f = cast(TypeFunction)f.copy();
            f.isreturn = true;
            f.isreturnscope = cast(bool) (funcdecl.storage_class & STC.returnScope);
            if (funcdecl.storage_class & STC.returninferred)
                f.isreturninferred = true;
        }
    }

    if (!funcdecl.inferScope)
        return;
    funcdecl.inferScope = false;

    // Eliminate maybescope's
    {
        // Create and fill array[] with maybe candidates from the `this` and the parameters
        VarDeclaration[10] tmp = void;
        size_t dim = (funcdecl.vthis !is null) + (funcdecl.parameters ? funcdecl.parameters.length : 0);

        import dmd.common.string : SmallBuffer;
        auto sb = SmallBuffer!VarDeclaration(dim, tmp[]);
        VarDeclaration[] array = sb[];

        size_t n = 0;
        if (funcdecl.vthis)
            array[n++] = funcdecl.vthis;
        if (funcdecl.parameters)
        {
            foreach (v; *funcdecl.parameters)
            {
                array[n++] = v;
            }
        }
        eliminateMaybeScopes(array[0 .. n]);
    }

    // Infer STC.scope_
    if (funcdecl.parameters && !funcdecl.errors)
    {
        assert(f.parameterList.length == funcdecl.parameters.length);
        foreach (u, p; f.parameterList)
        {
            auto v = (*funcdecl.parameters)[u];
            if (!v.isScope() && inferScope(v))
            {
                //printf("Inferring scope for %s\n", v.toChars());
                p.storageClass |= STC.scope_ | STC.scopeinferred;
            }
        }
    }

    if (funcdecl.vthis)
    {
        inferScope(funcdecl.vthis);
        f.isScopeQual = funcdecl.vthis.isScope();
        f.isscopeinferred = !!(funcdecl.vthis.storage_class & STC.scopeinferred);
    }
}

/**********************************************
 * Have some variables that are maybescopes that were
 * assigned values from other maybescope variables.
 * Now that semantic analysis of the function is
 * complete, we can finalize this by turning off
 * maybescope for array elements that cannot be scope.
 *
 * $(TABLE2 Scope Table,
 * $(THEAD `va`, `v`,    =>,  `va` ,  `v`  )
 * $(TROW maybe, maybe,  =>,  scope,  scope)
 * $(TROW scope, scope,  =>,  scope,  scope)
 * $(TROW scope, maybe,  =>,  scope,  scope)
 * $(TROW maybe, scope,  =>,  scope,  scope)
 * $(TROW -    , -    ,  =>,  -    ,  -    )
 * $(TROW -    , maybe,  =>,  -    ,  -    )
 * $(TROW -    , scope,  =>,  error,  error)
 * $(TROW maybe, -    ,  =>,  scope,  -    )
 * $(TROW scope, -    ,  =>,  scope,  -    )
 * )
 * Params:
 *      array = array of variables that were assigned to from maybescope variables
 */
private void eliminateMaybeScopes(VarDeclaration[] array)
{
    enum log = false;
    if (log) printf("eliminateMaybeScopes()\n");
    bool changes;
    do
    {
        changes = false;
        foreach (va; array)
        {
            if (log) printf("  va = %s\n", va.toChars());
            if (!(va.maybeScope || va.isScope()))
            {
                if (va.maybes)
                {
                    foreach (v; *va.maybes)
                    {
                        if (log) printf("    v = %s\n", v.toChars());
                        if (v.maybeScope)
                        {
                            // v cannot be scope since it is assigned to a non-scope va
                            notMaybeScope(v, va);
                            if (!v.isReference())
                                v.storage_class &= ~(STC.return_ | STC.returninferred);
                            changes = true;
                        }
                    }
                }
            }
        }
    } while (changes);
}

/************************************************
 * Is type a reference to a mutable value?
 *
 * This is used to determine if an argument that does not have a corresponding
 * Parameter, i.e. a variadic argument, is a pointer to mutable data.
 * Params:
 *      t = type of the argument
 * Returns:
 *      true if it's a pointer (or reference) to mutable data
 */
bool isReferenceToMutable(Type t)
{
    t = t.baseElemOf();

    if (!t.isMutable() ||
        !t.hasPointers())
        return false;

    switch (t.ty)
    {
        case Tpointer:
            if (t.nextOf().isTypeFunction())
                break;
            goto case;

        case Tarray:
        case Taarray:
        case Tdelegate:
            if (t.nextOf().isMutable())
                return true;
            break;

        case Tclass:
            return true;        // even if the class fields are not mutable

        case Tstruct:
            // Have to look at each field
            foreach (VarDeclaration v; t.isTypeStruct().sym.fields)
            {
                if (v.storage_class & STC.ref_)
                {
                    if (v.type.isMutable())
                        return true;
                }
                else if (v.type.isReferenceToMutable())
                    return true;
            }
            break;

        default:
            assert(0);
    }
    return false;
}

/****************************************
 * Is parameter a reference to a mutable value?
 *
 * This is used if an argument has a corresponding Parameter.
 * The argument type is necessary if the Parameter is inout.
 * Params:
 *      p = Parameter to check
 *      t = type of corresponding argument
 * Returns:
 *      true if it's a pointer (or reference) to mutable data
 */
bool isReferenceToMutable(Parameter p, Type t)
{
    if (p.isReference())
    {
        if (p.type.isConst() || p.type.isImmutable())
            return false;
        if (p.type.isWild())
        {
            return t.isMutable();
        }
        return p.type.isMutable();
    }
    return isReferenceToMutable(p.type);
}

/// When checking lifetime for assignment `va=v`, the way `va` encloses `v`
private enum EnclosedBy
{
    none = 0,
    refVar, // `va` is a `ref` variable, which may link to a global variable
    global, // `va` is a global variable
    returnScope, // `va` is a scope variable that may be returned
    longerScope, // `va` is another scope variable declared earlier than `v`
}

/**********************************
 * Determine if `va` has a lifetime that lasts past
 * the destruction of `v`
 * Params:
 *     va = variable assigned to
 *     v = variable being assigned
 * Returns:
 *     The way `va` encloses `v` (if any)
 */
private EnclosedBy enclosesLifetimeOf(VarDeclaration va, VarDeclaration v)
{
    if (!va)
        return EnclosedBy.none;

    if (va.isDataseg())
        return EnclosedBy.global;

    if (va.isScope() && va.isReturn() && !v.isReturn())
        return EnclosedBy.returnScope;

    if (va.isReference() && va.isParameter())
        return EnclosedBy.refVar;

    assert(va.sequenceNumber != va.sequenceNumber.init);
    assert(v.sequenceNumber != v.sequenceNumber.init);
    if (va.sequenceNumber < v.sequenceNumber)
        return EnclosedBy.longerScope;

    return EnclosedBy.none;
}

/***************************************
 * Add variable `v` to maybes[]
 *
 * When a maybescope variable `v` is assigned to a maybescope variable `va`,
 * we cannot determine if `this` is actually scope until the semantic
 * analysis for the function is completed. Thus, we save the data
 * until then.
 * Params:
 *     v = a variable with `maybeScope == true` that was assigned to `this`
 */
private void addMaybe(VarDeclaration va, VarDeclaration v)
{
    //printf("add %s to %s's list of dependencies\n", v.toChars(), toChars());
    if (!va.maybes)
        va.maybes = new VarDeclarations();
    va.maybes.push(v);
}

// `setUnsafePreview` partially evaluated for dip1000
private bool setUnsafeDIP1000(Scope* sc, bool gag, Loc loc, const(char)* msg,
    RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
{
    return setUnsafePreview(sc, global.params.useDIP1000, gag, loc, msg, arg0, arg1, arg2);
}

/***************************************
 * Check that taking the address of `v` is `@safe`
 *
 * It's not possible to take the address of a scope variable, because `scope` only applies
 * to the top level indirection.
 *
 * Params:
 *     v = variable that a reference is created
 *     e = expression that takes the referene
 *     sc = used to obtain function / deprecated status
 *     gag = don't print errors
 * Returns:
 *     true if taking the address of `v` is problematic because of the lack of transitive `scope`
 */
private bool checkScopeVarAddr(VarDeclaration v, Expression e, Scope* sc, bool gag)
{
    if (v.storage_class & STC.temp)
        return false;

    if (!v.isScope())
    {
        notMaybeScope(v, e);
        return false;
    }

    if (!e.type)
        return false;

    // When the type after dereferencing has no pointers, it's okay.
    // Comes up when escaping `&someStruct.intMember` of a `scope` struct:
    // scope does not apply to the `int`
    Type t = e.type.baseElemOf();
    if ((t.ty == Tarray || t.ty == Tpointer) && !t.nextOf().toBasetype().hasPointers())
        return false;

    // take address of `scope` variable not allowed, requires transitive scope
    return sc.setUnsafeDIP1000(gag, e.loc,
        "cannot take address of `scope` variable `%s` since `scope` applies to first indirection only", v);
}

/****************************
 * Determine if `v` is a typesafe variadic array, which is implicitly `scope`
 * Params:
 *      v = variable to check
 * Returns:
 *      true if `v` is a variadic parameter
 */
private bool isTypesafeVariadicArray(VarDeclaration v)
{
    if (v.storage_class & STC.variadic)
    {
        Type tb = v.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
            return true;
    }
    return false;
}
