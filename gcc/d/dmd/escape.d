/**
 * Most of the logic to implement scoped pointers and scoped references is here.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/escape.d, _escape.d)
 * Documentation:  https://dlang.org/phobos/dmd_escape.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/escape.d
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
import dmd.funcsem;
import dmd.globals : FeatureState;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.printast;
import dmd.rootobject;
import dmd.safe;
import dmd.tokens;
import dmd.typesem : hasPointers, parameterStorageClass;
import dmd.visitor;
import dmd.arraytypes;

private:

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
public
bool checkMutableArguments(ref Scope sc, FuncDeclaration fd, TypeFunction tf,
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
        VarDeclarations byref;
        VarDeclarations byvalue;
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
            eb.byref.push(var);
            continue;
        }

        void onRef(VarDeclaration v, bool transition) { eb.byref.push(v); }
        void onValue(VarDeclaration v) { eb.byvalue.push(v); }
        void onFunc(FuncDeclaration fd, bool called) {}
        void onExp(Expression e, bool transition) {}

        scope EscapeByResults er = EscapeByResults(&onRef, &onValue, &onFunc, &onExp);

        if (refs)
            escapeByRef(arg, er);
        else
            escapeByValue(arg, er);
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

        if (!tf.isLive && !(sc.useDIP1000 == FeatureState.enabled && sc.func && setFunctionToUnsafe(sc.func)))
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
            sc.eSink.error((*arguments)[i].loc, msg,
                  referenceVerb,
                  v.toChars(),
                  fd ? fd.toPrettyChars() : "indirectly");
        }
        errors = true;
    }

    void escape(size_t i, ref EscapeBy eb, bool byval)
    {
        foreach (VarDeclaration v; byval ? eb.byvalue : eb.byref)
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
                foreach (VarDeclaration v2; byval ? eb2.byvalue : eb2.byref)
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
public
bool checkArrayLiteralEscape(ref Scope sc, ArrayLiteralExp ae, bool gag)
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
public
bool checkAssocArrayLiteralEscape(ref Scope sc, AssocArrayLiteralExp ae, bool gag)
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
private
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
public
bool checkParamArgumentEscape(ref Scope sc, FuncDeclaration fdc, Identifier parId, VarDeclaration vPar, STC parStc, Expression arg, bool assertmsg, bool gag)
{
    enum log = false;
    if (log) printf("checkParamArgumentEscape(arg: %s par: %s parSTC: %llx)\n",
        arg ? arg.toChars() : "null",
        parId ? parId.toChars() : "null", parStc);
    //printf("type = %s, %d\n", arg.type.toChars(), arg.type.hasPointers());

    if (!arg.type.hasPointers())
        return false;

    bool result = false;

    /* 'v' is assigned unsafely to 'par'
     */
    void unsafeAssign(string desc)(VarDeclaration v)
    {
        if (assertmsg)
        {
            result |= sc.setUnsafeDIP1000(gag, arg.loc,
                "assigning" ~ desc ~ " `%s` to non-scope parameter calling `assert()`", v);
            return;
        }

        bool isThis = fdc && fdc.needThis() && fdc.vthis == vPar; // implicit `this` parameter to member function

        const(char)* msg =
            (isThis)        ? (desc ~ " `%s` calling non-scope member function `%s.%s()`") :
            (fdc &&  parId) ? ("assigning " ~ desc ~ " `%s` to non-scope parameter `%s` calling `%s`") :
            (fdc && !parId) ? ("assigning " ~ desc ~ " `%s` to non-scope anonymous parameter calling `%s`") :
            (!fdc && parId) ? ("assigning " ~ desc ~ " `%s` to non-scope parameter `%s`") :
            (desc ~ " `%s` assigned to non-scope anonymous parameter");

        if (isThis ?
            sc.setUnsafeDIP1000(gag, arg.loc, msg, arg, fdc.toParent2(), fdc) :
            sc.setUnsafeDIP1000(gag, arg.loc, msg, v, parId ? parId : fdc, fdc))
        {
            result = true;
            printScopeFailure(previewSupplementalFunc(sc.isDeprecated(), sc.useDIP1000), vPar, 10);
        }
    }

    void onValue(VarDeclaration v)
    {
        if (log) printf("byvalue %s\n", v.toChars());
        if (parStc & STC.scope_)
            return;

        doNotInferScope(v, vPar);

        if (v.isScope())
        {
            unsafeAssign!"scope variable"(v);
        }
    }

    void onRef(VarDeclaration v, bool retRefTransition)
    {
        if (log) printf("byref %s\n", v.toChars());

        Dsymbol p = v.toParent2();

        doNotInferScope(v, arg);
        if (checkScopeVarAddr(v, arg, sc, gag))
        {
            result = true;
            return;
        }

        if (p == sc.func && !(parStc & STC.scope_))
        {
            unsafeAssign!"reference to local variable"(v);
            return;
        }
    }

    void onFunc(FuncDeclaration fd, bool called)
    {
        //printf("fd = %s, %d\n", fd.toChars(), fd.tookAddressOf);
        if (parStc & STC.scope_)
            return;
        VarDeclarations vars;
        findAllOuterAccessedVariables(fd, &vars);

        foreach (v; vars)
        {
            //printf("v = %s\n", v.toChars());
            assert(!v.isDataseg());     // these are not put in the closureVars[]

            Dsymbol p = v.toParent2();

            doNotInferScope(v, arg);

            if ((v.isReference() || v.isScope()) && p == sc.func)
            {
                unsafeAssign!"reference to local"(v);
                return;
            }
        }
    }

    void onExp(Expression ee, bool retRefTransition)
    {
        if (parStc & STC.scope_)
            return;
        const(char)* msg = parId ?
            "assigning reference to stack allocated value returned by `%s` to non-scope parameter `%s`" :
            "assigning reference to stack allocated value returned by `%s` to non-scope anonymous parameter";

        result |= sc.setUnsafeDIP1000(gag, ee.loc, msg, ee, parId);
    }

    scope EscapeByResults er = EscapeByResults(&onRef, &onValue, &onFunc, &onExp);
    escapeByValue(arg, er);
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
public
bool checkParamArgumentReturn(ref Scope sc, Expression firstArg, Expression arg, Parameter param, bool gag)
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

    auto e = new AssignExp(arg.loc, firstArg, arg);
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
public
bool checkConstructorEscape(ref Scope sc, CallExp ce, bool gag)
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
public
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
public
ReturnParamDest returnParamDest(TypeFunction tf, Type tthis)
{
    assert(tf);
    if (tf.isCtor)
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
public
bool checkAssignEscape(ref Scope sc, Expression e, bool gag, bool byRef)
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


    /* The struct literal case can arise from the S(e2) constructor call:
     *    return S(e2);
     * and appears in this function as:
     *    structLiteral = e2;
     * Such an assignment does not necessarily remove scope-ness.
     */
    if (e1.isStructLiteralExp())
        return false;

    int deref;
    VarDeclaration va = expToVariable(e1, deref);
    // transitive scope not implemented, so can't assign scope pointers to a dereferenced variable
    if (deref > 0)
        va = null;

    if (e1.isSliceExp())
    {
        // slice-copy is not assigning a pointer, but copying array content
        if (va)
        {
            if (!va.type.toBasetype().isTypeSArray() || // treat static array slice same as a variable
                !va.type.hasPointers())
                return false;
        }
        else
            return false;
    }

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

    if (e.op == EXP.construct && va && (va.storage_class & STC.temp) && va._init)
    {
        // Initializing a temporary is safe, `escapeExp` will forward such vars
        // to their `va._init` if needed.
        return false;
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
                // While you'd expect fd.parameters[0] to exist in this case, the compiler-generated
                // expression that initializes an `out int* p = null` is analyzed before fd.parameters
                // is created, so we still do a null and length check
                vaIsFirstRef = fd.parameters && 0 < fd.parameters.length && (*fd.parameters)[0] == va;
                break;
            case ReturnParamDest.returnVal:
                break;
        }
    }
    if (log && vaIsFirstRef) printf("va is first ref `%s`\n", va.toChars());

    bool result = false;
    void onValue(VarDeclaration v)
    {
        if (log) printf("byvalue: %s\n", v.toChars());

        if (v == va)
            return;

        Dsymbol p = v.toParent2();

        if (vaIsFirstRef && p == fd)
        {
            inferReturn(fd, v, /*returnScope:*/ true);
        }

        if (!(va && va.isScope()) || vaIsRef)
            doNotInferScope(v, e);

        if (v.isScope())
        {
            if (vaIsFirstRef && v.isParameter() && v.isReturn())
            {
                // va=v, where v is `return scope`
                if (inferScope(va))
                    return;
            }

            // If va's lifetime encloses v's, then error
            if (EnclosedBy eb = va.enclosesLifetimeOf(v))
            {
                const(char)* msg;
                final switch (eb)
                {
                    case EnclosedBy.none: assert(0);
                    case EnclosedBy.returnScope:
                        msg = "assigning scope variable `%s` to return scope `%s`";
                        break;
                    case EnclosedBy.longerScope:
                        msg = "assigning scope variable `%s` to `%s` with longer lifetime";
                        break;
                    case EnclosedBy.refVar:
                        msg = "assigning scope variable `%s` to `ref` variable `%s` with longer lifetime";
                        break;
                    case EnclosedBy.global:
                        msg = "assigning scope variable `%s` to global variable `%s`";
                        break;
                }

                if (sc.setUnsafeDIP1000(gag, ae.loc, msg, v, va))
                {
                    result = true;
                    return;
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
                return;
            }
            result |= sc.setUnsafeDIP1000(gag, ae.loc, "assigning scope variable `%s` to non-scope `%s`", v, e1);
        }
        else
        {
            /* v is not 'scope', and we didn't check the scope of where we assigned it to.
             * It may escape via that assignment, therefore, v can never be 'scope'.
             */
            //printf("no infer for %s in %s, %d\n", v.toChars(), fd.ident.toChars(), __LINE__);
            if (!v.isParameter)
                doNotInferScope(v, e);
        }
    }

    void onRef(VarDeclaration v, bool retRefTransition)
    {
        if (log) printf("byref: %s\n", v.toChars());

        if (checkScopeVarAddr(v, ae, sc, gag))
        {
            result = true;
            return;
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
                    "assigning address of local variable `%s` to return scope `%s`", v, va);
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
            if (sc.setUnsafeDIP1000(gag, ae.loc, "assigning address of variable `%s` to `%s` with longer lifetime", v, va))
            {
                result = true;
                return;
            }
        }

        if (!(va && va.isScope()))
            doNotInferScope(v, e);

        if (p != sc.func)
            return;

        if (inferScope(va))
        {
            if (v.isReturn() && !va.isReturn())
                va.storage_class |= STC.return_ | STC.returninferred;
            return;
        }

        result |= sc.setUnsafeDIP1000(gag, ae.loc, "assigning reference to local variable `%s` to non-scope `%s`", v, e1);
    }

    void onFunc(FuncDeclaration func, bool called)
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
                doNotInferScope(v, e);

            if (!(v.isReference() || v.isScope()) || p != fd)
                return;

            if (va && !va.isDataseg() && (va.isScope() || va.maybeScope))
            {
                /* Don't infer STC.scope_ for va, because then a closure
                 * won't be generated for fd.
                 */
                //if (!va.isScope())
                    //va.storage_class |= STC.scope_ | STC.scopeinferred;
                return;
            }
            result |= sc.setUnsafeDIP1000(gag, ae.loc,
                "assigning reference to local `%s` to non-scope `%s`", v, e1);
        }
    }

    void onExp(Expression ee, bool retRefTransition)
    {
        if (log) printf("byexp: %s\n", ee.toChars());

        /* Do not allow slicing of a static array returned by a function
         */
        if (ee.op == EXP.call && ee.type.toBasetype().isTypeSArray() && e1.type.toBasetype().isTypeDArray())
        {
            if (!gag)
                sc.eSink.deprecation(ee.loc, "slice of static array temporary returned by `%s` assigned to longer lived variable `%s`",
                    ee.toChars(), e1.toChars());
            //result = true;
            return;
        }

        const(char)* msg = (ee.op == EXP.structLiteral) ?
            "assigning address of struct literal `%s`  to `%s` with longer lifetime" :
            "assigning address of expression temporary returned by `%s` to `%s` with longer lifetime";

        result |= sc.setUnsafeDIP1000(gag, ee.loc, msg, ee, e1);
    }

    scope EscapeByResults er = EscapeByResults(&onRef, &onValue, &onFunc, &onExp);

    if (byRef)
        escapeByRef(e2, er);
    else
        escapeByValue(e2, er);

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
public
bool checkThrowEscape(ref Scope sc, Expression e, bool gag)
{
    //printf("[%s] checkThrowEscape, e = %s\n", e.loc.toChars(), e.toChars());

    bool result = false;
    void onRef(VarDeclaration v, bool retRefTransition) {}
    void onValue(VarDeclaration v)
    {
        //printf("byvalue %s\n", v.toChars());
        if (v.isScope() && !v.iscatchvar)       // special case: allow catch var to be rethrown
                                                // despite being `scope`
        {
            // https://issues.dlang.org/show_bug.cgi?id=17029
            result |= sc.setUnsafeDIP1000(gag, e.loc, "throwing scope variable `%s`", v);
            return;
        }
        else
        {
            doNotInferScope(v, new ThrowExp(e.loc, e));
        }
    }
    void onFunc(FuncDeclaration fd, bool called) {}
    void onExp(Expression exp, bool retRefTransition) {}

    scope EscapeByResults er = EscapeByResults(&onRef, &onValue, &onFunc, &onExp);
    escapeByValue(e, er);
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
public
bool checkNewEscape(ref Scope sc, Expression e, bool gag)
{
    import dmd.globals: FeatureState;
    import dmd.errors: previewErrorFunc;

    //printf("[%s] checkNewEscape, e = %s\n", e.loc.toChars(), e.toChars());
    enum log = false;
    if (log) printf("[%s] checkNewEscape, e: `%s`\n", e.loc.toChars(), e.toChars());

    bool result = false;
    void onValue(VarDeclaration v)
    {
        if (log) printf("byvalue `%s`\n", v.toChars());

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
                result |= sc.setUnsafeDIP1000(gag, e.loc, "copying scope variable `%s` into allocated memory", v);
                return;
            }
        }
        else
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            doNotInferScope(v, e);
        }
    }

    void onRef(VarDeclaration v, bool retRefTransition)
    {
        if (log) printf("byref `%s`\n", v.toChars());

        // 'featureState' tells us whether to emit an error or a deprecation,
        // depending on the flag passed to the CLI for DIP25 / DIP1000
        bool escapingRef(VarDeclaration v, FeatureState fs)
        {
            const(char)* msg = v.isParameter() ?
                "escaping a reference to parameter `%s` by copying `%s` into allocated memory" :
                "escaping a reference to local variable `%s` by copying `%s` into allocated memory";
            return setUnsafePreview(&sc, fs, gag, e.loc, msg, v, e);
        }

        Dsymbol p = v.toParent2();

        if (!v.isReference())
        {
            if (p == sc.func)
            {
                result |= escapingRef(v, sc.useDIP1000);
                return;
            }
        }

        /* Check for returning a ref variable by 'ref', but should be 'return ref'
         * Infer the addition of 'return', or set result to be the offending expression.
         */
        if (!v.isReference())
            return;

        // https://dlang.org/spec/function.html#return-ref-parameters
        if (p == sc.func)
        {
            //printf("escaping reference to local ref variable %s\n", v.toChars());
            //printf("storage class = x%llx\n", v.storage_class);
            result |= escapingRef(v, sc.useDIP25);
            return;
        }
        // Don't need to be concerned if v's parent does not return a ref
        FuncDeclaration func = p.isFuncDeclaration();
        if (!func || !func.type)
            return;
        if (auto tf = func.type.isTypeFunction())
        {
            if (!tf.isRef)
                return;

            const(char)* msg = "storing reference to outer local variable `%s` into allocated memory causes it to escape";
            if (!gag)
            {
                previewErrorFunc(sc.isDeprecated(), sc.useDIP25)(e.loc, msg, v.toChars());
            }

            // If -preview=dip25 is used, the user wants an error
            // Otherwise, issue a deprecation
            result |= (sc.useDIP25 == FeatureState.enabled);
        }
    }

    void onFunc(FuncDeclaration fd, bool called)
    {
        if (called)
            result |= sc.setUnsafeDIP1000(gag, e.loc,
                "escaping a `scope` value returned from nested function `%s` into allocated memory", fd);
    }

    void onExp(Expression ee, bool retRefTransition)
    {
        if (log) printf("byexp %s\n", ee.toChars());
        if (!gag)
            sc.eSink.error(ee.loc, "escaping reference to stack allocated value returned by `%s` into allocated memory",
                  ee.toChars());
        result = true;
    }

    scope EscapeByResults er = EscapeByResults(&onRef, &onValue, &onFunc, &onExp);
    escapeByValue(e, er);

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
public
bool checkReturnEscape(ref Scope sc, Expression e, bool gag)
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
public
bool checkReturnEscapeRef(ref Scope sc, Expression e, bool gag)
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
private bool checkReturnEscapeImpl(ref Scope sc, Expression e, bool refs, bool gag)
{
    enum log = false;
    if (log) printf("[%s] checkReturnEscapeImpl, refs: %d e: `%s`\n", e.loc.toChars(), refs, e.toChars());

    bool result = false;
    void onValue(VarDeclaration v)
    {
        if (log) printf("byvalue `%s`\n", v.toChars());

        const vsr = buildScopeRef(v.storage_class);

        Dsymbol p = v.toParent2();

        if (p == sc.func && inferReturn(sc.func, v, /*returnScope:*/ true))
        {
            return;
        }

        if (v.isTypesafeVariadicArray && p == sc.func)
        {
            if (!gag)
                sc.eSink.error(e.loc, "returning `%s` escapes a reference to variadic parameter `%s`", e.toChars(), v.toChars());
            result = false;
        }
        else if (v.isScope())
        {
            /* If `return scope` applies to v.
             */
            if (vsr == ScopeRef.ReturnScope ||
                vsr == ScopeRef.Ref_ReturnScope)
            {
                return;
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
                !(!refs && p.parent == sc.func && pfunc.fes)
               )
            {
                /*
                 *  auto p(scope string s) {
                 *      string scfunc() { return s; }
                 *  }
                 */
                if (sc.func.isFuncDeclaration().getLevel(pfunc, sc.intypeof) > 0 &&
                    inferReturn(sc.func, sc.func.vthis, /*returnScope*/ !refs))
                {
                    return;
                }

                if (v.isParameter() && !v.isReturn())
                {
                    // https://issues.dlang.org/show_bug.cgi?id=23191
                    if (!gag)
                    {
                        previewErrorFunc(sc.isDeprecated(), sc.useDIP1000)(e.loc,
                            "scope parameter `%s` may not be returned", v.toChars()
                        );
                        result = true;
                        return;
                    }
                }
                else
                {
                    // https://issues.dlang.org/show_bug.cgi?id=17029
                    result |= sc.setUnsafeDIP1000(gag, e.loc, "returning scope variable `%s`", v);
                    return;
                }
            }
        }
        else if (p == sc.func || !v.isParameter())
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            doNotInferScope(v, e);
        }
    }

    void onRef(VarDeclaration v, bool retRefTransition)
    {
        if (log)
        {
            printf("byref `%s` %s\n", v.toChars(), ScopeRefToChars(buildScopeRef(v.storage_class)));
        }

        // 'featureState' tells us whether to emit an error or a deprecation,
        // depending on the flag passed to the CLI for DIP25
        void escapingRef(VarDeclaration v, FeatureState featureState)
        {
            const(char)* safeMsg = v.isParameter() ?
                "escaping a reference to parameter `%s` by returning `%s`" :
                "escaping a reference to local variable `%s` by returning `%s` ";
            if (v.isParameter() && v.isReference())
            {
                if (setUnsafePreview(&sc, featureState, gag, e.loc, safeMsg, v, e) ||
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
                if (retRefTransition)
                {
                    result |= sc.setUnsafeDIP1000(gag, e.loc, safeMsg, v, e);
                }
                else
                {
                    const(char)* msg = v.isParameter() ?
                        "returning `%s` escapes a reference to parameter `%s`" :
                        "returning `%s` escapes a reference to local variable `%s`";
                    if (!gag)
                        previewErrorFunc(sc.isDeprecated(), featureState)(e.loc, msg, e.toChars(), v.toChars());
                    result = true;
                }
            }
        }

        const vsr = buildScopeRef(v.storage_class);

        Dsymbol p = v.toParent2();

        // https://issues.dlang.org/show_bug.cgi?id=19965
        if (!refs && checkScopeVarAddr(v, e, sc, gag))
        {
            result = true;
            return;
        }

        if (!v.isReference())
        {
            if (p == sc.func)
            {
                escapingRef(v, FeatureState.enabled);
                return;
            }
            FuncDeclaration fd = p.isFuncDeclaration();
            if (fd && sc.func.scopeInprocess)
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
        if (vsr == ScopeRef.Ref ||
             vsr == ScopeRef.RefScope ||
             vsr == ScopeRef.Ref_ReturnScope)
        {
            if (p == sc.func && (vsr == ScopeRef.Ref || vsr == ScopeRef.RefScope) &&
                inferReturn(sc.func, v, /*returnScope:*/ false))
            {
                return;
            }
            else
            {
                // https://dlang.org/spec/function.html#return-ref-parameters
                // Only look for errors if in module listed on command line
                if (p == sc.func)
                {
                    //printf("escaping reference to local ref variable %s\n", v.toChars());
                    //printf("storage class = x%llx\n", v.storage_class);
                    escapingRef(v, sc.useDIP25);
                    return;
                }
                // Don't need to be concerned if v's parent does not return a ref
                FuncDeclaration fd = p.isFuncDeclaration();
                if (fd && fd.type && fd.type.ty == Tfunction)
                {
                    TypeFunction tf = fd.type.isTypeFunction();
                    if (tf.isRef)
                    {
                        const(char)* msg = "escaping reference to outer local variable `%s`";
                        if (!gag)
                            previewErrorFunc(sc.isDeprecated(), sc.useDIP25)(e.loc, msg, v.toChars());
                        result = true;
                        return;
                    }
                }

            }
        }
    }

    void onFunc(FuncDeclaration fd, bool called)
    {
        if (called && fd.isNested())
            result |= sc.setUnsafeDIP1000(gag, e.loc, "escaping local variable through nested function `%s`", fd);
    }

    void onExp(Expression ee, bool retRefTransition)
    {
        if (log) printf("byexp %s\n", ee.toChars());
        if (retRefTransition)
        {
            result |= sc.setUnsafeDIP1000(gag, ee.loc,
                "escaping reference to stack allocated value returned by `%s`", ee);
        }
        else
        {
            if (!gag)
                sc.eSink.error(ee.loc, "escaping reference to stack allocated value returned by `%s`", ee.toChars());
            result = true;
        }
    }


    scope EscapeByResults er = EscapeByResults(&onRef, &onValue, &onFunc, &onExp);

    if (refs)
        escapeByRef(e, er);
    else
        escapeByValue(e, er);

    return result;
}

/***********************************
 * Infer `scope` for a variable
 *
 * Params:
 *      va = variable to infer scope for
 * Returns: `true` if succesful or already `scope`
 */
private
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

    if (!fd.scopeInprocess)
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
            tf.isReturnScope = returnScope;
            tf.isReturn = true;
            tf.isReturnInferred = true;
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
 */
public
void escapeByValue(Expression e, ref scope EscapeByResults er)
{
    escapeExp(e, er, 0);
}

// Unified implementation of `escapeByValue` and `escapeByRef`
// deref = derference level, if `p` has deref 0, then `*p` has deref 1, `&p` has -1, and `**p` has 2 etc.
// For escapeByValue, deref = 0
// For escapeByRef, deref = -1
// Currently, `scope` is not transitive, so deref > 0 means no escaping, but `@live` does do transitive checking,
// and future enhancements might add some form of transitive scope.
void escapeExp(Expression e, ref scope EscapeByResults er, int deref)
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
        if (deref == 0 && e.e1.op != EXP.structLiteral)
            escapeExp(e.e1, er, deref - 1);
    }

    void visitSymOff(SymOffExp e)
    {
        if (VarDeclaration v = e.var.isVarDeclaration())
            er.varDeref(v, deref - 1);
    }

    void visitVar(VarExp e)
    {
        if (auto v = e.var.isVarDeclaration())
        {
            const refAddr = deref < 0 && v.storage_class & STC.ref_ ;
            const tempVar = deref == 0 && v.storage_class & STC.temp;
            if ((refAddr || tempVar) && v._init && v != er.lastTemp)
            {
                // If compiler generated ref temporary
                //   (ref v = ex; ex)
                // e.g. to extract side effects of `Tuple!(int, int).modify().expand[0]`
                // look at the initializer instead
                if (ExpInitializer ez = v._init.isExpInitializer())
                {
                    // Prevent endless loops. Consider:
                    // `__field0 = (S __tup1 = S(x, y);) , __field0 = __tup1.__fields_field_0`
                    // escapeExp would recurse on the lhs of the last assignment, which is __field0
                    // again. In this case, we want the rhs.
                    // Also consider appending a struct with a `return scope` constructor:
                    // __appendtmp34 = __appendtmp34.this(null)
                    // In that case we just break the cycle using `lastTemp`.
                    auto lc = ez.exp.lastComma();
                    auto restoreLastTemp = er.lastTemp;
                    er.lastTemp = v;
                    // printf("%s %s    TO    %s\n", e.loc.toChars, e.toChars, lc.toChars);
                    if (lc.isAssignExp || lc.isConstructExp || lc.isBlitExp)
                        escapeExp(lc.isBinExp().e2, er, deref);
                    else
                        escapeExp(ez.exp, er, deref);

                    er.lastTemp = restoreLastTemp;
                    return;
                }
            }

            if (deref < 0 || e.type.hasPointers())
                er.varDeref(v, deref);
        }
    }

    void visitThis(ThisExp e)
    {
        // Special case because `__this2` isn't `ref` internally
        if (deref == -1 && e.var && e.var.toParent2().isFuncDeclaration().hasDualContext())
        {
            escapeByValue(e, er);
            return;
        }

        if (e.var)
            er.varDeref(e.var, deref);
    }

    void visitPtr(PtrExp e)
    {
        if (deref < 0 || (er.live && e.type.hasPointers()))
            escapeExp(e.e1, er, deref + 1);
    }

    void visitDotVar(DotVarExp e)
    {
        auto t1b = e.e1.type.toBasetype();
        // Accessing a class field dereferences the `this` pointer
        if (t1b.isTypeClass())
            escapeExp(e.e1, er, deref + 1);
        else if (deref < 0 || e.type.hasPointers())
            escapeExp(e.e1, er, deref);
    }

    void visitDelegate(DelegateExp e)
    {
        Type t = e.e1.type.toBasetype();
        if (t.isTypeClass() || t.isTypePointer())
            escapeByValue(e.e1, er);
        else
            escapeByRef(e.e1, er);
        er.byFunc(e.func, false);
    }

    void visitFunc(FuncExp e)
    {
        if (e.fd.tok == TOK.delegate_)
            er.byFunc(e.fd, false);
    }

    void visitTuple(TupleExp e)
    {
        assert(0); // should have been lowered by now
    }

    void visitArrayLiteral(ArrayLiteralExp e)
    {
        Type tb = e.type.toBasetype();
        if (tb.isStaticOrDynamicArray())
        {
            if (e.basis)
                escapeExp(e.basis, er, deref);
            foreach (el; *e.elements)
            {
                if (el)
                    escapeExp(el, er, deref);
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
                    escapeExp(ex, er, deref);
            }
        }
        if (deref == -1)
        {
            er.byExp(e, er.inRetRefTransition > 0); //
        }
    }

    void visitNew(NewExp e)
    {
        if (e.placement)
            escapeExp(e.placement, er, deref);

        Type tb = e.newtype.toBasetype();
        if (tb.isTypeStruct() && !e.member && e.arguments)
        {
            foreach (ex; *e.arguments)
            {
                if (ex)
                    escapeExp(ex, er, deref);
            }
        }
    }

    void visitCast(CastExp e)
    {
        if (deref < 0 || !e.type.hasPointers())
            return;
        Type tb = e.type.toBasetype();
        if (tb.isTypeDArray() && e.e1.type.toBasetype().isTypeSArray())
            escapeExp(e.e1, er, deref - 1);
        else
            escapeExp(e.e1, er, deref);
    }

    void visitSlice(SliceExp e)
    {
        // Usually: slicing a static array escapes by ref, slicing a dynamic array escapes by value.
        // However, slices with compile-time known length can implicitly converted to static arrays:
        // int*[3] b = sa[0 .. 3];
        // So we need to compare the type before slicing and after slicing
        const bool staticBefore = e.e1.type.toBasetype().isTypeSArray() !is null;
        const bool staticAfter = e.type.toBasetype().isTypeSArray() !is null;
        escapeExp(e.e1, er, deref + staticAfter - staticBefore);
    }

    void visitIndex(IndexExp e)
    {
        Type tb = e.e1.type.toBasetype();

        if (tb.isTypeSArray())
        {
            escapeExp(e.e1, er, deref);
        }
        else if (tb.isTypeDArray())
        {
            escapeExp(e.e1, er, deref + 1);
        }
    }

    void visitBin(BinExp e)
    {
        if (e.type.toBasetype().isTypePointer())
        {
            // The expression must be pointer arithmetic, e.g. `p + 1` or `1 + p`
            escapeExp(e.e1, er, deref);
            escapeExp(e.e2, er, deref);
        }
    }

    void visitBinAssign(BinAssignExp e)
    {
        escapeExp(e.e1, er, deref);
    }

    void visitAssign(AssignExp e)
    {
        escapeExp(e.e1, er, deref);
    }

    void visitComma(CommaExp e)
    {
        escapeExp(e.e2, er, deref);
    }

    void visitCond(CondExp e)
    {
        escapeExp(e.e1, er, deref);
        escapeExp(e.e2, er, deref);
    }

    void visitCall(CallExp e)
    {
        //printf("CallExp(): %s\n", e.toChars());
        // Check each argument that is passed as 'return scope'.
        TypeFunction tf = e.calledFunctionType();
        if (!tf)
            return;

        if (deref < 0 && !tf.isRef)
        {
            er.byExp(e, er.inRetRefTransition > 0);
            return;
        }

        // A function may have a return scope struct parameter, but only return an `int` field of that struct
        if (deref >= 0 && !e.type.hasPointers())
            return;

        /// Given a `scope` / `return scope` / `return ref` annotation,
        /// get the corresponding pointer dereference level
        static int paramDeref(ScopeRef psr)
        {
            return
                (psr == ScopeRef.ReturnRef || psr == ScopeRef.ReturnRef_Scope) ? -1 :
                (psr == ScopeRef.ReturnScope || psr == ScopeRef.Ref_ReturnScope) ? 0 :
                +1;
        }

        if (e.arguments && e.arguments.length)
        {
            // j=1 if _arguments[] is first argument,
            // skip it because it is not passed by ref
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

                    // For struct constructors, `tf.isRef` is true, but for escape analysis,
                    // it's as if they return `void` and escape through the first (`this`) parameter:
                    // void assign(ref S this, return scope constructorArgs...)
                    // If you then return the constructed result by value, it doesn't count
                    // as dereferencing the scope arguments, they're still escaped.
                    const isRef = tf.isRef && !(tf.isCtor && paramDeref(psr) == 0);
                    const maybeInaccurate = deref == 0 && paramDeref(psr) == 0;
                    er.inRetRefTransition += maybeInaccurate;
                    if (paramDeref(psr) <= 0)
                        escapeExp(arg, er, deref + paramDeref(psr) + isRef);
                    er.inRetRefTransition -= maybeInaccurate;
                }
            }
        }

        // If 'this' is returned, check it too
        Type t1 = e.e1.type.toBasetype();
        DotVarExp dve = e.e1.isDotVarExp();
        if (dve && t1.ty == Tfunction)
        {
            FuncDeclaration fd = dve.var.isFuncDeclaration();
            if (!fd)
                return;

            // https://issues.dlang.org/show_bug.cgi?id=20149#c10
            if (deref < 0 && dve.var.isCtorDeclaration())
            {
                er.byExp(e, false);
                return;
            }

            // Calling a non-static member function dve.var, which is returning `this`, and with dve.e1 representing `this`
            const psr = buildScopeRef(getThisStorageClass(fd));
            er.inRetRefTransition += (psr == ScopeRef.ReturnRef_Scope);
            if (paramDeref(psr) <= 0)
                escapeExp(dve.e1, er, deref + paramDeref(psr) + (tf.isRef && !tf.isCtor));
            er.inRetRefTransition -= (psr == ScopeRef.ReturnRef_Scope);
        }

        // The return value of a delegate call with return (scope) may point to a closure variable,
        // so escape the delegate in case it's `scope` / stack allocated.
        if (t1.isTypeDelegate() && tf.isReturn)
        {
            escapeExp(e.e1, er, deref + tf.isRef);
        }

        // If `fd` is a nested function that's return ref / return scope, check that
        // it doesn't escape closure vars
        if (auto ve = e.e1.isVarExp())
        {
            if (FuncDeclaration fd = ve.var.isFuncDeclaration())
            {
                if (fd.isNested() && tf.isReturn)
                {
                    er.byFunc(fd, true);
                }
            }
        }
    }

    if (deref > 0 && !er.live)
        return; // scope is not transitive currently, so dereferencing expressions don't escape

    if (deref >= 0 && er.live && !e.type.hasPointers())
        return; // can't escape non-pointer values by value

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
            if (auto ba = e.isBinAssignExp())
                return visitBinAssign(ba);
            if (auto b = e.isBinExp())
                return visitBin(b);
            return visit(e);
    }
}

/*****************************
 * Concoct storage class for member function's implicit `this` parameter.
 * Params:
 *      fd = member function
 * Returns:
 *      storage class for fd's `this`
 */
STC getThisStorageClass(FuncDeclaration fd)
{
    STC stc;
    auto tf = fd.type.toBasetype().isTypeFunction();
    if (tf.isReturn)
        stc |= STC.return_;
    if (tf.isReturnScope)
        stc |= STC.returnScope | STC.scope_;
    auto ad = fd.isThis();
    if ((ad && ad.isClassDeclaration()) || tf.isScopeQual)
        stc |= STC.scope_;
    if (ad && ad.isStructDeclaration())
        stc |= STC.ref_;        // `this` for a struct member function is passed by `ref`
    return stc;
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
 */
void escapeByRef(Expression e, ref scope EscapeByResults er)
{
    escapeExp(e, er, -1);
}

/************************************
 * Aggregate the data collected by the escapeBy??() functions.
 */
public
struct EscapeByResults
{
    /*
     * retRefTransition = Whether the variable / expression went through a `return (ref) scope` function call
     *
     * This is needed for the dip1000 by default transition, since the rules for
     * disambiguating `return scope ref` have changed. Therefore, functions in legacy code
     * can be mistakenly treated as `return ref` making the compiler believe stack variables
     * are being escaped, which is an error even in `@system` code. By keeping track of this
     * information, variables escaped through `return ref` can be treated as a deprecation instead
     * of error, see test/fail_compilation/dip1000_deprecation.d
     *
     * Additionally, return scope can be inferred wrongly instead of scope, in which
     * case the code could give false positives even without @safe or dip1000:
     * https://issues.dlang.org/show_bug.cgi?id=23657
     */

    /// called on variables being returned by ref / address
    void delegate(VarDeclaration, bool retRefTransition) byRef;
    /// called on variables with values containing pointers
    void delegate(VarDeclaration) byValue;

    /// Switch over `byValue` and `byRef` based on `deref` level (-1 = by ref, 0 = by value, 1 = only for live currently)
    private void varDeref(VarDeclaration var, int deref)
    {
        if (var.isDataseg())
            return;
        if (deref == -1)
            byRef(var, inRetRefTransition > 0);
        else if (deref == 0)
            byValue(var);
        else if (deref > 0 && live)
            byValue(var);
    }

    /// called on nested functions that are turned into delegates
    /// When `called` is true, it means the delegate escapes variables
    /// from the closure through a call to it, while `false` means the
    /// delegate itself escapes.
    void delegate(FuncDeclaration, bool called) byFunc;
    /// called when expression temporaries are being returned by ref / address
    void delegate(Expression, bool retRefTransition) byExp;

    /// if @live semantics apply, i.e. expressions `p`, `*p`, `**p`, etc., all return `p`.
    bool live = false;

    /// Incremented / decremented every time an ambiguous return ref/scope parameter is checked.
    /// See retRefTransition above.
    private int inRetRefTransition = 0;

    /// When forwarding a temp var to its initializer,
    /// keep track of the temp var to break endless loops
    private VarDeclaration lastTemp = null;
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
private void doNotInferScope(VarDeclaration v, RootObject o)
{
    if (v.maybeScope)
    {
        v.maybeScope = false;
        if (o && v.isParameter())
            EscapeState.scopeInferFailure[v.sequenceNumber] = o;
    }
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
public
void finishScopeParamInference(FuncDeclaration funcdecl, ref TypeFunction f)
{
    if (!funcdecl.scopeInprocess)
        return;
    funcdecl.scopeInprocess = false;

    if (funcdecl.storage_class & STC.return_)
    {
        if (funcdecl.type == f)
            f = cast(TypeFunction)f.copy();
        f.isReturn = true;
        f.isReturnScope = cast(bool) (funcdecl.storage_class & STC.returnScope);
        if (funcdecl.storage_class & STC.returninferred)
            f.isReturnInferred = true;
    }


    // Infer STC.scope_
    if (funcdecl.parameters && !funcdecl.errors)
    {
        assert(f.parameterList.length == funcdecl.parameters.length);
        foreach (u, p; f.parameterList)
        {
            auto v = (*funcdecl.parameters)[u];
            if (!v.isScope() && v.type.hasPointers() && inferScope(v))
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
        f.isScopeInferred = !!(funcdecl.vthis.storage_class & STC.scopeinferred);
    }
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
private
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

        case Tnull:
            return false;

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
private
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

// `setUnsafePreview` partially evaluated for dip1000
public
bool setUnsafeDIP1000(ref Scope sc, bool gag, Loc loc, const(char)* msg,
    RootObject[] args...)
{
    return setUnsafePreview(&sc, sc.useDIP1000, gag, loc, msg, args);
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
private bool checkScopeVarAddr(VarDeclaration v, Expression e, ref Scope sc, bool gag)
{
    if (!v.isScope())
    {
        doNotInferScope(v, e);
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
        "taking address of `scope` variable `%s` with pointers", v);
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
        if (tb.isStaticOrDynamicArray())
            return true;
    }
    return false;
}
