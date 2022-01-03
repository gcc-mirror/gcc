/**
 * Most of the logic to implement scoped pointers and scoped references is here.
 *
 * Copyright:   Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
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
import dmd.mtype;
import dmd.printast;
import dmd.root.rootobject;
import dmd.tokens;
import dmd.visitor;
import dmd.arraytypes;

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

    /* Store escapeBy as static data escapeByStorage so we can keep reusing the same
     * arrays rather than reallocating them.
     */
    __gshared EscapeBy[] escapeByStorage;
    auto escapeBy = escapeByStorage;
    if (escapeBy.length < len)
    {
        auto newPtr = cast(EscapeBy*)mem.xrealloc(escapeBy.ptr, len * EscapeBy.sizeof);
        // Clear the new section
        memset(newPtr + escapeBy.length, 0, (len - escapeBy.length) * EscapeBy.sizeof);
        escapeBy = newPtr[0 .. len];
        escapeByStorage = escapeBy;
    }
    else
        escapeBy = escapeBy[0 .. len];

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
            eb.er.byref.push(var);
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

        if (!(global.params.useDIP1000 == FeatureState.enabled && sc.func.setUnsafe()))
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

    /* Reset the arrays in escapeBy[] so we can reuse them next time through
     */
    foreach (ref eb; escapeBy)
    {
        eb.er.reset();
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

/****************************************
 * Function parameter `par` is being initialized to `arg`,
 * and `par` may escape.
 * Detect if scoped values can escape this way.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      fdc = function being called, `null` if called indirectly
 *      par = function parameter (`this` if null)
 *      arg = initializer for param
 *      assertmsg = true if the parameter is the msg argument to assert(bool, msg).
 *      gag = do not print error messages
 * Returns:
 *      `true` if pointers to the stack can escape via assignment
 */
bool checkParamArgumentEscape(Scope* sc, FuncDeclaration fdc, Parameter par, Expression arg, bool assertmsg, bool gag)
{
    enum log = false;
    if (log) printf("checkParamArgumentEscape(arg: %s par: %s)\n",
        arg ? arg.toChars() : "null",
        par ? par.toChars() : "this");
    //printf("type = %s, %d\n", arg.type.toChars(), arg.type.hasPointers());

    if (!arg.type.hasPointers())
        return false;

    EscapeByResults er;

    escapeByValue(arg, &er);

    if (!er.byref.dim && !er.byvalue.dim && !er.byfunc.dim && !er.byexp.dim)
        return false;

    bool result = false;

    ScopeRef psr;
    if (par && fdc && fdc.type.isTypeFunction())
        psr = buildScopeRef(par.storageClass);
    else
        psr = ScopeRef.None;

    /* 'v' is assigned unsafely to 'par'
     */
    void unsafeAssign(VarDeclaration v, const char* desc)
    {
        if (global.params.useDIP1000 == FeatureState.enabled && sc.func.setUnsafe())
        {
            if (!gag)
            {
                if (assertmsg)
                {
                    error(arg.loc, "%s `%s` assigned to non-scope parameter calling `assert()`",
                        desc, v.toChars());
                }
                else
                {
                    error(arg.loc, "%s `%s` assigned to non-scope parameter `%s` calling %s",
                        desc, v.toChars(),
                        par ? par.toChars() : "this",
                        fdc ? fdc.toPrettyChars() : "indirectly");
                }
            }
            result = true;
        }
    }

    foreach (VarDeclaration v; er.byvalue)
    {
        if (log) printf("byvalue %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        notMaybeScope(v);

        if (v.isScope())
        {
            unsafeAssign(v, "scope variable");
        }
        else if (v.storage_class & STC.variadic && p == sc.func)
        {
            Type tb = v.type.toBasetype();
            if (tb.ty == Tarray || tb.ty == Tsarray)
            {
                unsafeAssign(v, "variadic variable");
            }
        }
        else
        {
            /* v is not 'scope', and is assigned to a parameter that may escape.
             * Therefore, v can never be 'scope'.
             */
            if (log) printf("no infer for %s in %s loc %s, fdc %s, %d\n",
                v.toChars(), sc.func.ident.toChars(), sc.func.loc.toChars(), fdc.ident.toChars(),  __LINE__);
            v.doNotInferScope = true;
        }
    }

    foreach (VarDeclaration v; er.byref)
    {
        if (log) printf("byref %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        notMaybeScope(v);

        if (!v.isReference() && p == sc.func)
        {
            if (psr == ScopeRef.Scope ||
                psr == ScopeRef.RefScope ||
                psr == ScopeRef.ReturnRef_Scope)
            {
                continue;
            }

            unsafeAssign(v, "reference to local variable");
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

            notMaybeScope(v);

            if ((v.isReference() || v.isScope()) && p == sc.func)
            {
                unsafeAssign(v, "reference to local");
                continue;
            }
        }
    }

    foreach (Expression ee; er.byexp)
    {
        if (sc.func && sc.func.setUnsafe())
        {
            if (!gag)
                error(ee.loc, "reference to stack allocated value returned by `%s` assigned to non-scope parameter `%s`",
                    ee.toChars(),
                    par ? par.toChars() : "this");
            result = true;
        }
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
 *      gag = do not print error messages
 * Returns:
 *      `true` if assignment to `firstArg` would cause an error
 */
bool checkParamArgumentReturn(Scope* sc, Expression firstArg, Expression arg, bool gag)
{
    enum log = false;
    if (log) printf("checkParamArgumentReturn(firstArg: %s arg: %s)\n",
        firstArg.toChars(), arg.toChars());
    //printf("type = %s, %d\n", arg.type.toChars(), arg.type.hasPointers());

    if (!arg.type.hasPointers())
        return false;

    scope e = new AssignExp(arg.loc, firstArg, arg);
    return checkAssignEscape(sc, e, gag);
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

    if (!ce.arguments && ce.arguments.dim)
        return false;

    DotVarExp dve = ce.e1.isDotVarExp();
    CtorDeclaration ctor = dve.var.isCtorDeclaration();
    TypeFunction tf = ctor.type.isTypeFunction();

    const nparams = tf.parameterList.length;
    const n = ce.arguments.dim;

    // j=1 if _arguments[] is first argument
    const j = tf.isDstyleVariadic();

    /* Attempt to assign each `return` arg to the `this` reference
     */
    foreach (const i; 0 .. n)
    {
        Expression arg = (*ce.arguments)[i];
        if (!arg.type.hasPointers())
            return false;

        //printf("\targ[%d]: %s\n", i, arg.toChars());

        if (i - j < nparams && i >= j)
        {
            Parameter p = tf.parameterList[i - j];

            if (p.storageClass & STC.return_)
            {
                /* Fake `dve.e1 = arg;` and look for scope violations
                 */
                scope e = new AssignExp(arg.loc, dve.e1, arg);
                if (checkAssignEscape(sc, e, gag))
                    return true;
            }
        }
    }

    return false;
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
 * Returns:
 *      `true` if pointers to the stack can escape via assignment
 */
bool checkAssignEscape(Scope* sc, Expression e, bool gag)
{
    enum log = false;
    if (log) printf("checkAssignEscape(e: %s)\n", e.toChars());
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
        return false;

    /* The struct literal case can arise from the S(e2) constructor call:
     *    return S(e2);
     * and appears in this function as:
     *    structLiteral = e2;
     * Such an assignment does not necessarily remove scope-ness.
     */
    if (e1.isStructLiteralExp())
        return false;

    EscapeByResults er;

    escapeByValue(e2, &er);

    if (!er.byref.dim && !er.byvalue.dim && !er.byfunc.dim && !er.byexp.dim)
        return false;

    VarDeclaration va = expToVariable(e1);

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


    // Determine if va is a parameter that is an indirect reference
    const bool vaIsRef = va && va.storage_class & STC.parameter &&
        (va.isReference() || va.type.toBasetype().isTypeClass()); // ref, out, or class
    if (log && vaIsRef) printf("va is ref `%s`\n", va.toChars());

    /* Determine if va is the first parameter, through which other 'return' parameters
     * can be assigned.
     * This works the same as returning the value via a return statement.
     * Although va is marked as `ref`, it is not regarded as returning by `ref`.
     * https://dlang.org.spec/function.html#return-ref-parameters
     */
    bool isFirstRef()
    {
        if (!vaIsRef)
            return false;
        Dsymbol p = va.toParent2();
        if (p == fd && fd.type && fd.type.isTypeFunction())
        {
            TypeFunction tf = fd.type.isTypeFunction();
            if (!tf.nextOf() || (tf.nextOf().ty != Tvoid && !fd.isCtorDeclaration()))
                return false;
            if (va == fd.vthis) // `this` of a non-static member function is considered to be the first parameter
                return true;
            if (fd.parameters && fd.parameters.length && (*fd.parameters)[0] == va) // va is first parameter
                return true;
        }
        return false;
    }
    const bool vaIsFirstRef = isFirstRef();
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
            (va.storage_class & v.storage_class & (STC.maybescope | STC.variadic)) == STC.maybescope &&
            p == fd)
        {
            /* Add v to va's list of dependencies
             */
            va.addMaybe(v);
            continue;
        }

        if (vaIsFirstRef &&
            (v.isScope() || (v.storage_class & STC.maybescope)) &&
            !(v.storage_class & STC.return_) &&
            v.isParameter() &&
            fd.flags & FUNCFLAG.returnInprocess &&
            p == fd)
        {
            if (log) printf("inferring 'return' for parameter %s in function %s\n", v.toChars(), fd.toChars());
            inferReturn(fd, v);        // infer addition of 'return' to make `return scope`
        }

        if (!(va && va.isScope()) || vaIsRef)
            notMaybeScope(v);

        if (v.isScope())
        {
            if (vaIsFirstRef && v.isParameter() && v.storage_class & STC.return_)
            {
                // va=v, where v is `return scope`
                if (va.isScope())
                    continue;

                if (!va.doNotInferScope)
                {
                    if (log) printf("inferring scope for lvalue %s\n", va.toChars());
                    va.storage_class |= STC.scope_ | STC.scopeinferred;
                    continue;
                }
            }

            if (va && va.isScope() && va.storage_class & STC.return_ && !(v.storage_class & STC.return_) &&
                fd.setUnsafe())
            {
                // va may return its value, but v does not allow that, so this is an error
                if (!gag)
                    error(ae.loc, "scope variable `%s` assigned to return scope `%s`", v.toChars(), va.toChars());
                result = true;
                continue;
            }

            // If va's lifetime encloses v's, then error
            if (va &&
                (va.enclosesLifetimeOf(v) && !(v.storage_class & (STC.parameter | STC.temp)) ||
                 // va is class reference
                 ae.e1.isDotVarExp() && va.type.toBasetype().isTypeClass() && (va.enclosesLifetimeOf(v) ||
                 !va.isScope()) ||
                 vaIsRef ||
                 va.isReference() && !(v.storage_class & (STC.parameter | STC.temp))) &&
                fd.setUnsafe())
            {
                if (!gag)
                    error(ae.loc, "scope variable `%s` assigned to `%s` with longer lifetime", v.toChars(), va.toChars());
                result = true;
                continue;
            }

            if (va && !va.isDataseg() && !va.doNotInferScope)
            {
                if (!va.isScope())
                {   /* v is scope, and va is not scope, so va needs to
                     * infer scope
                     */
                    if (log) printf("inferring scope for %s\n", va.toChars());
                    va.storage_class |= STC.scope_ | STC.scopeinferred;
                    /* v returns, and va does not return, so va needs
                     * to infer return
                     */
                    if (v.storage_class & STC.return_ &&
                        !(va.storage_class & STC.return_))
                    {
                        if (log) printf("infer return for %s\n", va.toChars());
                        va.storage_class |= STC.return_ | STC.returninferred;

                        // Added "return scope" so don't confuse it with "return ref"
                        if (isRefReturnScope(va.storage_class))
                            va.storage_class |= STC.returnScope;
                    }
                }
                continue;
            }
            if (fd.setUnsafe())
            {
                if (!gag)
                    error(ae.loc, "scope variable `%s` assigned to non-scope `%s`", v.toChars(), e1.toChars());
                result = true;
            }
        }
        else if (v.storage_class & STC.variadic && p == fd)
        {
            Type tb = v.type.toBasetype();
            if (tb.ty == Tarray || tb.ty == Tsarray)
            {
                if (va && !va.isDataseg() && !va.doNotInferScope)
                {
                    if (!va.isScope())
                    {   //printf("inferring scope for %s\n", va.toChars());
                        va.storage_class |= STC.scope_ | STC.scopeinferred;
                    }
                    continue;
                }
                if (fd.setUnsafe())
                {
                    if (!gag)
                        error(ae.loc, "variadic variable `%s` assigned to non-scope `%s`", v.toChars(), e1.toChars());
                    result = true;
                }
            }
        }
        else
        {
            /* v is not 'scope', and we didn't check the scope of where we assigned it to.
             * It may escape via that assignment, therefore, v can never be 'scope'.
             */
            //printf("no infer for %s in %s, %d\n", v.toChars(), fd.ident.toChars(), __LINE__);
            v.doNotInferScope = true;
        }
    }

ByRef:
    foreach (VarDeclaration v; er.byref)
    {
        if (log) printf("byref: %s\n", v.toChars());
        if (v.isDataseg())
            continue;

        if (global.params.useDIP1000 == FeatureState.enabled)
        {
            if (va && va.isScope() && !v.isReference())
            {
                if (!(va.storage_class & STC.return_))
                {
                    va.doNotInferReturn = true;
                }
                else if (fd.setUnsafe())
                {
                    if (!gag)
                        error(ae.loc, "address of local variable `%s` assigned to return scope `%s`", v.toChars(), va.toChars());
                    result = true;
                    continue;
                }
            }
        }

        Dsymbol p = v.toParent2();

        // If va's lifetime encloses v's, then error
        if (va &&
            (va.enclosesLifetimeOf(v) && !(v.isParameter() && v.isRef()) ||
             va.isDataseg()) &&
            fd.setUnsafe())
        {
            if (!gag)
                error(ae.loc, "address of variable `%s` assigned to `%s` with longer lifetime", v.toChars(), va.toChars());
            result = true;
            continue;
        }

        if (va && v.isReference())
        {
            Dsymbol pva = va.toParent2();
            for (Dsymbol pv = p; pv; )
            {
                pv = pv.toParent2();
                if (pva == pv)  // if v is nested inside pva
                {
                    if (fd.setUnsafe())
                    {
                        if (!gag)
                            error(ae.loc, "reference `%s` assigned to `%s` with longer lifetime", v.toChars(), va.toChars());
                        result = true;
                        continue ByRef;
                    }
                    break;
                }
            }
        }

        if (!(va && va.isScope()))
            notMaybeScope(v);

        if ((global.params.useDIP1000 != FeatureState.enabled && v.isReference()) || p != sc.func)
            continue;

        if (va && !va.isDataseg() && !va.doNotInferScope)
        {
            if (!va.isScope())
            {   //printf("inferring scope for %s\n", va.toChars());
                va.storage_class |= STC.scope_ | STC.scopeinferred;
            }
            if (v.storage_class & STC.return_ && !(va.storage_class & STC.return_))
                va.storage_class |= STC.return_ | STC.returninferred;
            continue;
        }
        if (e1.op == EXP.structLiteral)
            continue;
        if (fd.setUnsafe())
        {
            if (!gag)
                error(ae.loc, "reference to local variable `%s` assigned to non-scope `%s`", v.toChars(), e1.toChars());
            result = true;
        }
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
        if (va && va.isScope() && !(va.storage_class & STC.return_) && func.tookAddressOf)
            --func.tookAddressOf;

        foreach (v; vars)
        {
            //printf("v = %s\n", v.toChars());
            assert(!v.isDataseg());     // these are not put in the closureVars[]

            Dsymbol p = v.toParent2();

            if (!(va && va.isScope()))
                notMaybeScope(v);

            if (!(v.isReference() || v.isScope()) || p != fd)
                continue;

            if (va && !va.isDataseg() && !va.doNotInferScope)
            {
                /* Don't infer STC.scope_ for va, because then a closure
                 * won't be generated for fd.
                 */
                //if (!va.isScope())
                    //va.storage_class |= STC.scope_ | STC.scopeinferred;
                continue;
            }
            if (fd.setUnsafe())
            {
                if (!gag)
                    error(ae.loc, "reference to local `%s` assigned to non-scope `%s` in @safe code", v.toChars(), e1.toChars());
                result = true;
            }
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
            (!va || !(va.storage_class & STC.temp)) &&
            fd.setUnsafe())
        {
            if (!gag)
                error(ee.loc, "address of struct temporary returned by `%s` assigned to longer lived variable `%s`",
                    ee.toChars(), e1.toChars());
            result = true;
            continue;
        }

        if (ee.op == EXP.structLiteral &&
            (!va || !(va.storage_class & STC.temp)) &&
            fd.setUnsafe())
        {
            if (!gag)
                error(ee.loc, "address of struct literal `%s` assigned to longer lived variable `%s`",
                    ee.toChars(), e1.toChars());
            result = true;
            continue;
        }

        if (va && !va.isDataseg() && !va.doNotInferScope)
        {
            if (!va.isScope())
            {   //printf("inferring scope for %s\n", va.toChars());
                va.storage_class |= STC.scope_ | STC.scopeinferred;
            }
            continue;
        }

        if (fd.setUnsafe())
        {
            if (!gag)
                error(ee.loc, "reference to stack allocated value returned by `%s` assigned to non-scope `%s`",
                    ee.toChars(), e1.toChars());
            result = true;
        }
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

    if (!er.byref.dim && !er.byvalue.dim && !er.byexp.dim)
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
            if (global.params.useDIP1000 == FeatureState.enabled) // https://issues.dlang.org/show_bug.cgi?id=17029
            {
                if (!gag)
                    error(e.loc, "scope variable `%s` may not be thrown", v.toChars());
                result = true;
            }
            continue;
        }
        else
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            v.doNotInferScope = true;
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

    if (!er.byref.dim && !er.byvalue.dim && !er.byexp.dim)
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
                // Only look for errors if in module listed on command line
                if (global.params.useDIP1000 == FeatureState.enabled   // https://issues.dlang.org/show_bug.cgi?id=17029
                    && sc.func.setUnsafe())     // https://issues.dlang.org/show_bug.cgi?id=20868
                {
                    if (!gag)
                        error(e.loc, "scope variable `%s` may not be copied into allocated memory", v.toChars());
                    result = true;
                }
                continue;
            }
        }
        else if (v.storage_class & STC.variadic && p == sc.func)
        {
            Type tb = v.type.toBasetype();
            if (tb.ty == Tarray || tb.ty == Tsarray)
            {
                if (!gag)
                    error(e.loc, "copying `%s` into allocated memory escapes a reference to variadic parameter `%s`", e.toChars(), v.toChars());
                result = false;
            }
        }
        else
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            v.doNotInferScope = true;
        }
    }

    foreach (VarDeclaration v; er.byref)
    {
        if (log) printf("byref `%s`\n", v.toChars());

        // 'featureState' tells us whether to emit an error or a deprecation,
        // depending on the flag passed to the CLI for DIP25
        void escapingRef(VarDeclaration v, FeatureState featureState = FeatureState.enabled)
        {
            if (!gag)
            {
                const(char)* kind = (v.storage_class & STC.parameter) ? "parameter" : "local";
                const(char)* msg = "copying `%s` into allocated memory escapes a reference to %s variable `%s`";
                previewErrorFunc(sc.isDeprecated(), featureState)(e.loc, msg, e.toChars(), kind, v.toChars());
            }
            result |= (featureState == FeatureState.enabled);
        }

        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        if (!v.isReference())
        {
            if (p == sc.func)
            {
                escapingRef(v);
                continue;
            }
        }

        /* Check for returning a ref variable by 'ref', but should be 'return ref'
         * Infer the addition of 'return', or set result to be the offending expression.
         */
        if (!v.isReference())
            continue;

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

    if (!er.byref.dim && !er.byvalue.dim && !er.byexp.dim)
        return false;

    bool result = false;
    foreach (VarDeclaration v; er.byvalue)
    {
        if (log) printf("byvalue `%s`\n", v.toChars());
        if (v.isDataseg())
            continue;

        Dsymbol p = v.toParent2();

        if ((v.isScope() || (v.storage_class & STC.maybescope)) &&
            !(v.storage_class & STC.return_) &&
            v.isParameter() &&
            !v.doNotInferReturn &&
            sc.func.flags & FUNCFLAG.returnInprocess &&
            p == sc.func)
        {
            inferReturn(sc.func, v);        // infer addition of 'return'
            continue;
        }

        if (v.isScope())
        {
            if (v.storage_class & STC.return_)
                continue;

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
                // Only look for errors if in module listed on command line
                // https://issues.dlang.org/show_bug.cgi?id=17029
                if (global.params.useDIP1000 == FeatureState.enabled && sc.func.setUnsafe())
                {
                    if (!gag)
                        error(e.loc, "scope variable `%s` may not be returned", v.toChars());
                    result = true;
                }
                continue;
            }
        }
        else if (v.storage_class & STC.variadic && p == sc.func)
        {
            Type tb = v.type.toBasetype();
            if (tb.ty == Tarray || tb.ty == Tsarray)
            {
                if (!gag)
                    error(e.loc, "returning `%s` escapes a reference to variadic parameter `%s`", e.toChars(), v.toChars());
                result = false;
            }
        }
        else
        {
            //printf("no infer for %s in %s, %d\n", v.toChars(), sc.func.ident.toChars(), __LINE__);
            v.doNotInferScope = true;
        }
    }

    foreach (VarDeclaration v; er.byref)
    {
        if (log)
        {
            printf("byref `%s`\n", v.toChars());
            if (v.storage_class & STC.return_) printf(" return");
            if (v.storage_class & STC.ref_)    printf(" ref");
            if (v.storage_class & STC.scope_)  printf(" scope");
            printf("\n");
        }

        // 'featureState' tells us whether to emit an error or a deprecation,
        // depending on the flag passed to the CLI for DIP25
        void escapingRef(VarDeclaration v, ScopeRef vsr, FeatureState featureState = FeatureState.enabled)
        {
            if (!gag)
            {
                const(char)* msg, supplemental;
                if (v.storage_class & STC.parameter &&
                    (v.type.hasPointers() || v.storage_class & STC.ref_))
                {
                    msg = "returning `%s` escapes a reference to parameter `%s`";
                    supplemental = vsr == ScopeRef.Ref_ReturnScope
                                              ? "perhaps remove `scope` parameter annotation so `return` applies to `ref`"
                                              : "perhaps annotate the parameter with `return`";
                }
                else
                {
                    msg = "returning `%s` escapes a reference to local variable `%s`";
                    if (v.ident is Id.This)
                        supplemental = "perhaps annotate the function with `return`";
                }

                previewErrorFunc(sc.isDeprecated(), featureState)(e.loc, msg, e.toChars(), v.toChars());
                if (supplemental)
                    previewSupplementalFunc(sc.isDeprecated(), featureState)(e.loc, supplemental);
            }
            result = true;
        }

        if (v.isDataseg())
            continue;

        const vsr = buildScopeRef(v.storage_class);

        Dsymbol p = v.toParent2();

        // https://issues.dlang.org/show_bug.cgi?id=19965
        if (!refs && sc.func.vthis == v)
            notMaybeScope(v);

        if (!v.isReference())
        {
            if (p == sc.func)
            {
                escapingRef(v, vsr, FeatureState.enabled);
                continue;
            }
            FuncDeclaration fd = p.isFuncDeclaration();
            if (fd && sc.func.flags & FUNCFLAG.returnInprocess)
            {
                /* Code like:
                 *   int x;
                 *   auto dg = () { return &x; }
                 * Making it:
                 *   auto dg = () return { return &x; }
                 * Because dg.ptr points to x, this is returning dt.ptr+offset
                 */
                if (global.params.useDIP1000 == FeatureState.enabled)
                {
                    sc.func.storage_class |= STC.return_ | STC.returninferred;
                }
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
            if (sc.func.flags & FUNCFLAG.returnInprocess && p == sc.func &&
                (vsr == ScopeRef.Ref || vsr == ScopeRef.RefScope))
            {
                inferReturn(sc.func, v);        // infer addition of 'return'
            }
            else
            {
                // https://dlang.org/spec/function.html#return-ref-parameters
                // Only look for errors if in module listed on command line
                if (p == sc.func)
                {
                    //printf("escaping reference to local ref variable %s\n", v.toChars());
                    //printf("storage class = x%llx\n", v.storage_class);
                    escapingRef(v, vsr, global.params.useDIP25);
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

    foreach (Expression ee; er.byexp)
    {
        if (log) printf("byexp %s\n", ee.toChars());
        if (!gag)
            error(ee.loc, "escaping reference to stack allocated value returned by `%s`", ee.toChars());
        result = true;
    }

    return result;
}


/*************************************
 * Variable v needs to have 'return' inferred for it.
 * Params:
 *      fd = function that v is a parameter to
 *      v = parameter that needs to be STC.return_
 */

private void inferReturn(FuncDeclaration fd, VarDeclaration v)
{
    // v is a local in the current function

    //printf("for function '%s' inferring 'return' for variable '%s'\n", fd.toChars(), v.toChars());
    v.storage_class |= STC.return_ | STC.returninferred;

    if (v == fd.vthis)
    {
        /* v is the 'this' reference, so mark the function
         */
        fd.storage_class |= STC.return_ | STC.returninferred;
        if (auto tf = fd.type.isTypeFunction())
        {
            //printf("'this' too %p %s\n", tf, sc.func.toChars());
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
                    p.storageClass |= STC.return_ | STC.returninferred;
                    break;              // there can be only one
                }
            }
        }
    }
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
 */
void escapeByValue(Expression e, EscapeByResults* er, bool live = false)
{
    //printf("[%s] escapeByValue, e: %s\n", e.loc.toChars(), e.toChars());
    extern (C++) final class EscapeVisitor : Visitor
    {
        alias visit = Visitor.visit;
    public:
        EscapeByResults* er;
        bool live;

        extern (D) this(EscapeByResults* er, bool live)
        {
            this.er = er;
            this.live = live;
        }

        override void visit(Expression e)
        {
        }

        override void visit(AddrExp e)
        {
            /* Taking the address of struct literal is normally not
             * allowed, but CTFE can generate one out of a new expression,
             * but it'll be placed in static data so no need to check it.
             */
            if (e.e1.op != EXP.structLiteral)
                escapeByRef(e.e1, er, live);
        }

        override void visit(SymOffExp e)
        {
            VarDeclaration v = e.var.isVarDeclaration();
            if (v)
                er.byref.push(v);
        }

        override void visit(VarExp e)
        {
            if (auto v = e.var.isVarDeclaration())
            {
                if (v.type.hasPointers() || // not tracking non-pointers
                    v.storage_class & STC.lazy_) // lazy variables are actually pointers
                    er.byvalue.push(v);
            }
        }

        override void visit(ThisExp e)
        {
            if (e.var)
                er.byvalue.push(e.var);
        }

        override void visit(PtrExp e)
        {
            if (live && e.type.hasPointers())
                e.e1.accept(this);
        }

        override void visit(DotVarExp e)
        {
            auto t = e.e1.type.toBasetype();
            if (e.type.hasPointers() && (live || t.ty == Tstruct))
            {
                e.e1.accept(this);
            }
        }

        override void visit(DelegateExp e)
        {
            Type t = e.e1.type.toBasetype();
            if (t.ty == Tclass || t.ty == Tpointer)
                escapeByValue(e.e1, er, live);
            else
                escapeByRef(e.e1, er, live);
            er.byfunc.push(e.func);
        }

        override void visit(FuncExp e)
        {
            if (e.fd.tok == TOK.delegate_)
                er.byfunc.push(e.fd);
        }

        override void visit(TupleExp e)
        {
            assert(0); // should have been lowered by now
        }

        override void visit(ArrayLiteralExp e)
        {
            Type tb = e.type.toBasetype();
            if (tb.ty == Tsarray || tb.ty == Tarray)
            {
                if (e.basis)
                    e.basis.accept(this);
                foreach (el; *e.elements)
                {
                    if (el)
                        el.accept(this);
                }
            }
        }

        override void visit(StructLiteralExp e)
        {
            if (e.elements)
            {
                foreach (ex; *e.elements)
                {
                    if (ex)
                        ex.accept(this);
                }
            }
        }

        override void visit(NewExp e)
        {
            Type tb = e.newtype.toBasetype();
            if (tb.ty == Tstruct && !e.member && e.arguments)
            {
                foreach (ex; *e.arguments)
                {
                    if (ex)
                        ex.accept(this);
                }
            }
        }

        override void visit(CastExp e)
        {
            if (!e.type.hasPointers())
                return;
            Type tb = e.type.toBasetype();
            if (tb.ty == Tarray && e.e1.type.toBasetype().ty == Tsarray)
            {
                escapeByRef(e.e1, er, live);
            }
            else
                e.e1.accept(this);
        }

        override void visit(SliceExp e)
        {
            if (auto ve = e.e1.isVarExp())
            {
                VarDeclaration v = ve.var.isVarDeclaration();
                Type tb = e.type.toBasetype();
                if (v)
                {
                    if (tb.ty == Tsarray)
                        return;
                    if (v.storage_class & STC.variadic)
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
                    escapeByRef(e.e1, er, live);
            }
            else
                e.e1.accept(this);
        }

        override void visit(IndexExp e)
        {
            if (e.e1.type.toBasetype().ty == Tsarray ||
                live && e.type.hasPointers())
            {
                e.e1.accept(this);
            }
        }

        override void visit(BinExp e)
        {
            Type tb = e.type.toBasetype();
            if (tb.ty == Tpointer)
            {
                e.e1.accept(this);
                e.e2.accept(this);
            }
        }

        override void visit(BinAssignExp e)
        {
            e.e1.accept(this);
        }

        override void visit(AssignExp e)
        {
            e.e1.accept(this);
        }

        override void visit(CommaExp e)
        {
            e.e2.accept(this);
        }

        override void visit(CondExp e)
        {
            e.e1.accept(this);
            e.e2.accept(this);
        }

        override void visit(CallExp e)
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

            if (e.arguments && e.arguments.dim)
            {
                /* j=1 if _arguments[] is first argument,
                 * skip it because it is not passed by ref
                 */
                int j = tf.isDstyleVariadic();
                for (size_t i = j; i < e.arguments.dim; ++i)
                {
                    Expression arg = (*e.arguments)[i];
                    size_t nparams = tf.parameterList.length;
                    if (i - j < nparams && i >= j)
                    {
                        Parameter p = tf.parameterList[i - j];
                        const stc = tf.parameterStorageClass(null, p);
                        if ((stc & (STC.scope_)) && (stc & STC.return_))
                            arg.accept(this);
                        else if ((stc & (STC.ref_)) && (stc & STC.return_))
                        {
                            if (tf.isref)
                            {
                                /* Treat:
                                 *   ref P foo(return ref P p)
                                 * as:
                                 *   p;
                                 */
                                arg.accept(this);
                            }
                            else
                                escapeByRef(arg, er, live);
                        }
                    }
                }
            }
            // If 'this' is returned, check it too
            if (e.e1.op == EXP.dotVariable && t1.ty == Tfunction)
            {
                DotVarExp dve = e.e1.isDotVarExp();
                FuncDeclaration fd = dve.var.isFuncDeclaration();
                AggregateDeclaration ad;
                if (global.params.useDIP1000 == FeatureState.enabled && tf.isreturn && fd && (ad = fd.isThis()) !is null)
                {
                    if (ad.isClassDeclaration() || tf.isScopeQual)       // this is 'return scope'
                        dve.e1.accept(this);
                    else if (ad.isStructDeclaration()) // this is 'return ref'
                    {
                        if (tf.isref)
                        {
                            /* Treat calling:
                             *   struct S { ref S foo() return; }
                             * as:
                             *   this;
                             */
                            dve.e1.accept(this);
                        }
                        else
                            escapeByRef(dve.e1, er, live);
                    }
                }
                else if (dve.var.storage_class & STC.return_ || tf.isreturn)
                {
                    if (dve.var.storage_class & STC.scope_)
                        dve.e1.accept(this);
                    else if (dve.var.storage_class & STC.ref_)
                        escapeByRef(dve.e1, er, live);
                }
                // If it's also a nested function that is 'return scope'
                if (fd && fd.isNested())
                {
                    if (tf.isreturn && tf.isScopeQual)
                        er.byexp.push(e);
                }
            }

            /* If returning the result of a delegate call, the .ptr
             * field of the delegate must be checked.
             */
            if (dg)
            {
                if (tf.isreturn)
                    e.e1.accept(this);
            }

            /* If it's a nested function that is 'return scope'
             */
            if (auto ve = e.e1.isVarExp())
            {
                FuncDeclaration fd = ve.var.isFuncDeclaration();
                if (fd && fd.isNested())
                {
                    if (tf.isreturn && tf.isScopeQual)
                        er.byexp.push(e);
                }
            }
        }
    }

    scope EscapeVisitor v = new EscapeVisitor(er, live);
    e.accept(v);
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
 */
void escapeByRef(Expression e, EscapeByResults* er, bool live = false)
{
    //printf("[%s] escapeByRef, e: %s\n", e.loc.toChars(), e.toChars());
    extern (C++) final class EscapeRefVisitor : Visitor
    {
        alias visit = Visitor.visit;
    public:
        EscapeByResults* er;
        bool live;

        extern (D) this(EscapeByResults* er, bool live)
        {
            this.er = er;
            this.live = live;
        }

        override void visit(Expression e)
        {
        }

        override void visit(VarExp e)
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
                            ce.e2.accept(this);
                        else
                            ez.exp.accept(this);
                    }
                }
                else
                    er.byref.push(v);
            }
        }

        override void visit(ThisExp e)
        {
            if (e.var && e.var.toParent2().isFuncDeclaration().isThis2)
                escapeByValue(e, er, live);
            else if (e.var)
                er.byref.push(e.var);
        }

        override void visit(PtrExp e)
        {
            escapeByValue(e.e1, er, live);
        }

        override void visit(IndexExp e)
        {
            Type tb = e.e1.type.toBasetype();
            if (auto ve = e.e1.isVarExp())
            {
                VarDeclaration v = ve.var.isVarDeclaration();
                if (tb.ty == Tarray || tb.ty == Tsarray)
                {
                    if (v && v.storage_class & STC.variadic)
                    {
                        er.byref.push(v);
                        return;
                    }
                }
            }
            if (tb.ty == Tsarray)
            {
                e.e1.accept(this);
            }
            else if (tb.ty == Tarray)
            {
                escapeByValue(e.e1, er, live);
            }
        }

        override void visit(StructLiteralExp e)
        {
            if (e.elements)
            {
                foreach (ex; *e.elements)
                {
                    if (ex)
                        ex.accept(this);
                }
            }
            er.byexp.push(e);
        }

        override void visit(DotVarExp e)
        {
            Type t1b = e.e1.type.toBasetype();
            if (t1b.ty == Tclass)
                escapeByValue(e.e1, er, live);
            else
                e.e1.accept(this);
        }

        override void visit(BinAssignExp e)
        {
            e.e1.accept(this);
        }

        override void visit(AssignExp e)
        {
            e.e1.accept(this);
        }

        override void visit(CommaExp e)
        {
            e.e2.accept(this);
        }

        override void visit(CondExp e)
        {
            e.e1.accept(this);
            e.e2.accept(this);
        }

        override void visit(CallExp e)
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
                if (e.arguments && e.arguments.dim)
                {
                    /* j=1 if _arguments[] is first argument,
                     * skip it because it is not passed by ref
                     */
                    int j = tf.isDstyleVariadic();
                    for (size_t i = j; i < e.arguments.dim; ++i)
                    {
                        Expression arg = (*e.arguments)[i];
                        size_t nparams = tf.parameterList.length;
                        if (i - j < nparams && i >= j)
                        {
                            Parameter p = tf.parameterList[i - j];
                            const stc = tf.parameterStorageClass(null, p);
                            if ((stc & (STC.out_ | STC.ref_)) && (stc & STC.return_))
                                arg.accept(this);
                            else if ((stc & STC.scope_) && (stc & STC.return_))
                            {
                                if (auto de = arg.isDelegateExp())
                                {
                                    if (de.func.isNested())
                                        er.byexp.push(de);
                                }
                                else
                                    escapeByValue(arg, er, live);
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
                        er.byexp.push(e);
                        return;
                    }

                    if (dve.var.storage_class & STC.return_ || tf.isreturn)
                    {
                        if (dve.var.storage_class & STC.ref_ || tf.isref)
                            dve.e1.accept(this);
                        else if (dve.var.storage_class & STC.scope_ || tf.isScopeQual)
                            escapeByValue(dve.e1, er, live);
                    }
                    // If it's also a nested function that is 'return ref'
                    FuncDeclaration fd = dve.var.isFuncDeclaration();
                    if (fd && fd.isNested())
                    {
                        if (tf.isreturn)
                            er.byexp.push(e);
                    }
                }
                // If it's a delegate, check it too
                if (e.e1.op == EXP.variable && t1.ty == Tdelegate)
                {
                    escapeByValue(e.e1, er, live);
                }

                /* If it's a nested function that is 'return ref'
                 */
                if (auto ve = e.e1.isVarExp())
                {
                    FuncDeclaration fd = ve.var.isFuncDeclaration();
                    if (fd && fd.isNested())
                    {
                        if (tf.isreturn)
                            er.byexp.push(e);
                    }
                }
            }
            else
                er.byexp.push(e);
        }
    }

    scope EscapeRefVisitor v = new EscapeRefVisitor(er, live);
    e.accept(v);
}


/************************************
 * Aggregate the data collected by the escapeBy??() functions.
 */
struct EscapeByResults
{
    VarDeclarations byref;      // array into which variables being returned by ref are inserted
    VarDeclarations byvalue;    // array into which variables with values containing pointers are inserted
    FuncDeclarations byfunc;    // nested functions that are turned into delegates
    Expressions byexp;          // array into which temporaries being returned by ref are inserted

    /** Reset arrays so the storage can be used again
     */
    void reset()
    {
        byref.setDim(0);
        byvalue.setDim(0);
        byfunc.setDim(0);
        byexp.setDim(0);
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
 * Turn off `STC.maybescope` for variable `v`.
 *
 * This exists in order to find where `STC.maybescope` is getting turned off.
 * Params:
 *      v = variable
 */
version (none)
{
    public void notMaybeScope(string file = __FILE__, int line = __LINE__)(VarDeclaration v)
    {
        printf("%.*s(%d): notMaybeScope('%s')\n", cast(int)file.length, file.ptr, line, v.toChars());
        v.storage_class &= ~STC.maybescope;
    }
}
else
{
    public void notMaybeScope(VarDeclaration v)
    {
        v.storage_class &= ~STC.maybescope;
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
public void eliminateMaybeScopes(VarDeclaration[] array)
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
            if (!(va.storage_class & (STC.maybescope | STC.scope_)))
            {
                if (va.maybes)
                {
                    foreach (v; *va.maybes)
                    {
                        if (log) printf("    v = %s\n", v.toChars());
                        if (v.storage_class & STC.maybescope)
                        {
                            // v cannot be scope since it is assigned to a non-scope va
                            notMaybeScope(v);
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
