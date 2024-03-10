/**
 * Semantic analysis of expressions.
 *
 * Specification: ($LINK2 https://dlang.org/spec/expression.html, Expressions)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/expressionsem.d, _expressionsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_expressionsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/expressionsem.d
 */

module dmd.expressionsem;

import core.stdc.stdio;

import dmd.access;
import dmd.aggregate;
import dmd.aliasthis;
import dmd.arrayop;
import dmd.arraytypes;
import dmd.attrib;
import dmd.astcodegen;
import dmd.astenums;
import dmd.canthrow;
import dmd.chkformat;
import dmd.ctorflow;
import dmd.dscope;
import dmd.dsymbol;
import dmd.declaration;
import dmd.dclass;
import dmd.dcast;
import dmd.delegatize;
import dmd.denum;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmangle;
import dmd.dmodule;
import dmd.dstruct;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.file_manager;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.imphint;
import dmd.importc;
import dmd.init;
import dmd.initsem;
import dmd.inline;
import dmd.intrange;
import dmd.location;
import dmd.mtype;
import dmd.mustuse;
import dmd.nspace;
import dmd.opover;
import dmd.optimize;
import dmd.parse;
import dmd.printast;
import dmd.root.array;
import dmd.root.ctfloat;
import dmd.root.file;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.rootobject;
import dmd.root.string;
import dmd.root.utf;
import dmd.semantic2;
import dmd.semantic3;
import dmd.sideeffect;
import dmd.safe;
import dmd.target;
import dmd.tokens;
import dmd.traits;
import dmd.typesem;
import dmd.typinf;
import dmd.utils;
import dmd.visitor;

enum LOGSEMANTIC = false;

/********************************************************
 * Perform semantic analysis and CTFE on expressions to produce
 * a string.
 * Params:
 *      buf = append generated string to buffer
 *      sc = context
 *      exps = array of Expressions
 * Returns:
 *      true on error
 */
bool expressionsToString(ref OutBuffer buf, Scope* sc, Expressions* exps)
{
    if (!exps)
        return false;

    foreach (ex; *exps)
    {
        if (!ex)
            continue;
        auto sc2 = sc.startCTFE();
        sc2.tinst = null;
        sc2.minst = null;       // prevents emission of any instantiated templates to object file
        auto e2 = ex.expressionSemantic(sc2);
        auto e3 = resolveProperties(sc2, e2);
        sc2.endCTFE();

        // allowed to contain types as well as expressions
        auto e4 = ctfeInterpretForPragmaMsg(e3);
        if (!e4 || e4.op == EXP.error)
            return true;

        // expand tuple
        if (auto te = e4.isTupleExp())
        {
            if (expressionsToString(buf, sc, te.exps))
                return true;
            continue;
        }
        // char literals exp `.toStringExp` return `null` but we cant override it
        // because in most contexts we don't want the conversion to succeed.
        IntegerExp ie = e4.isIntegerExp();
        const ty = (ie && ie.type) ? ie.type.ty : Terror;
        if (ty.isSomeChar)
        {
            auto tsa = new TypeSArray(ie.type, IntegerExp.literal!1);
            e4 = new ArrayLiteralExp(ex.loc, tsa, ie);
        }

        if (StringExp se = e4.toStringExp())
            buf.writestring(se.toUTF8(sc).peekString());
        else
            buf.writestring(e4.toString());
    }
    return false;
}


/***********************************************************
 * Resolve `exp` as a compile-time known string.
 * Params:
 *  sc  = scope
 *  exp = Expression which expected as a string
 *  s   = What the string is expected for, will be used in error diagnostic.
 * Returns:
 *  String literal, or `null` if error happens.
 */
StringExp semanticString(Scope *sc, Expression exp, const char* s)
{
    sc = sc.startCTFE();
    exp = exp.expressionSemantic(sc);
    exp = resolveProperties(sc, exp);
    sc = sc.endCTFE();

    if (exp.op == EXP.error)
        return null;

    auto e = exp;
    if (exp.type.isString())
    {
        e = e.ctfeInterpret();
        if (e.op == EXP.error)
            return null;
    }

    auto se = e.toStringExp();
    if (!se)
    {
        exp.error("`string` expected for %s, not `(%s)` of type `%s`",
            s, exp.toChars(), exp.type.toChars());
        return null;
    }
    return se;
}

private Expression extractOpDollarSideEffect(Scope* sc, UnaExp ue)
{
    Expression e0;
    Expression e1 = Expression.extractLast(ue.e1, e0);
    // https://issues.dlang.org/show_bug.cgi?id=12585
    // Extract the side effect part if ue.e1 is comma.

    if ((sc.flags & SCOPE.ctfe) ? hasSideEffect(e1) : !isTrivialExp(e1)) // match logic in extractSideEffect()
    {
        /* Even if opDollar is needed, 'e1' should be evaluate only once. So
         * Rewrite:
         *      e1.opIndex( ... use of $ ... )
         *      e1.opSlice( ... use of $ ... )
         * as:
         *      (ref __dop = e1, __dop).opIndex( ... __dop.opDollar ...)
         *      (ref __dop = e1, __dop).opSlice( ... __dop.opDollar ...)
         */
        e1 = extractSideEffect(sc, "__dop", e0, e1, false);
        assert(e1.isVarExp());
        e1.isVarExp().var.storage_class |= STC.exptemp;     // lifetime limited to expression
    }
    ue.e1 = e1;
    return e0;
}

/**************************************
 * Runs semantic on ae.arguments. Declares temporary variables
 * if '$' was used.
 */
Expression resolveOpDollar(Scope* sc, ArrayExp ae, Expression* pe0)
{
    assert(!ae.lengthVar);
    *pe0 = null;
    AggregateDeclaration ad = isAggregate(ae.e1.type);
    Dsymbol slice = search_function(ad, Id.slice);
    //printf("slice = %s %s\n", slice.kind(), slice.toChars());
    foreach (i, e; *ae.arguments)
    {
        if (i == 0)
            *pe0 = extractOpDollarSideEffect(sc, ae);

        if (e.op == EXP.interval && !(slice && slice.isTemplateDeclaration()))
        {
        Lfallback:
            if (ae.arguments.length == 1)
                return null;
            ae.error("multi-dimensional slicing requires template `opSlice`");
            return ErrorExp.get();
        }
        //printf("[%d] e = %s\n", i, e.toChars());

        // Create scope for '$' variable for this dimension
        auto sym = new ArrayScopeSymbol(sc, ae);
        sym.parent = sc.scopesym;
        sc = sc.push(sym);
        ae.lengthVar = null; // Create it only if required
        ae.currentDimension = i; // Dimension for $, if required

        e = e.expressionSemantic(sc);
        e = resolveProperties(sc, e);

        if (ae.lengthVar && sc.func)
        {
            // If $ was used, declare it now
            Expression de = new DeclarationExp(ae.loc, ae.lengthVar);
            de = de.expressionSemantic(sc);
            *pe0 = Expression.combine(*pe0, de);
        }
        sc = sc.pop();

        if (auto ie = e.isIntervalExp())
        {
            auto tiargs = new Objects();
            Expression edim = new IntegerExp(ae.loc, i, Type.tsize_t);
            edim = edim.expressionSemantic(sc);
            tiargs.push(edim);

            auto fargs = new Expressions(2);
            (*fargs)[0] = ie.lwr;
            (*fargs)[1] = ie.upr;

            uint xerrors = global.startGagging();
            sc = sc.push();
            FuncDeclaration fslice = resolveFuncCall(ae.loc, sc, slice, tiargs, ae.e1.type, ArgumentList(fargs), FuncResolveFlag.quiet);
            sc = sc.pop();
            global.endGagging(xerrors);
            if (!fslice)
                goto Lfallback;

            e = new DotTemplateInstanceExp(ae.loc, ae.e1, slice.ident, tiargs);
            e = new CallExp(ae.loc, e, fargs);
            e = e.expressionSemantic(sc);
        }

        if (!e.type)
        {
            ae.error("`%s` has no value", e.toChars());
            e = ErrorExp.get();
        }
        if (e.op == EXP.error)
            return e;

        (*ae.arguments)[i] = e;
    }
    return ae;
}

/**************************************
 * Runs semantic on se.lwr and se.upr. Declares a temporary variable
 * if '$' was used.
 * Returns:
 *      ae, or ErrorExp if errors occurred
 */
Expression resolveOpDollar(Scope* sc, ArrayExp ae, IntervalExp ie, Expression* pe0)
{
    //assert(!ae.lengthVar);
    if (!ie)
        return ae;

    VarDeclaration lengthVar = ae.lengthVar;
    bool errors = false;

    // create scope for '$'
    auto sym = new ArrayScopeSymbol(sc, ae);
    sym.parent = sc.scopesym;
    sc = sc.push(sym);

    Expression sem(Expression e)
    {
        e = e.expressionSemantic(sc);
        e = resolveProperties(sc, e);
        if (!e.type)
        {
            ae.error("`%s` has no value", e.toChars());
            errors = true;
        }
        return e;
    }

    ie.lwr = sem(ie.lwr);
    ie.upr = sem(ie.upr);

    if (ie.lwr.isErrorExp() || ie.upr.isErrorExp())
        errors = true;

    if (lengthVar != ae.lengthVar && sc.func)
    {
        // If $ was used, declare it now
        Expression de = new DeclarationExp(ae.loc, ae.lengthVar);
        de = de.expressionSemantic(sc);
        *pe0 = Expression.combine(*pe0, de);
    }

    sc = sc.pop();

    return errors ? ErrorExp.get() : ae;
}

/******************************
 * Perform semantic() on an array of Expressions.
 */
extern(D) bool arrayExpressionSemantic(
    Expression[] exps, Scope* sc, bool preserveErrors = false)
{
    bool err = false;
    foreach (ref e; exps)
    {
        if (e is null) continue;
        auto e2 = e.expressionSemantic(sc);
        if (e2.op == EXP.error)
            err = true;
        if (preserveErrors || e2.op != EXP.error)
            e = e2;
    }
    return err;
}

/*
Checks if `exp` contains a direct access to a `noreturn`
variable. If that is the case, an `assert(0)` expression
is generated and returned. This function should be called
only after semantic analysis has been performed on `exp`.

Params:
    exp = expression that is checked

Returns:
    An `assert(0)` expression if `exp` contains a `noreturn`
    variable access, `exp` otherwise.
*/

Expression checkNoreturnVarAccess(Expression exp)
{
    assert(exp.type);

    Expression result = exp;
    if (exp.type.isTypeNoreturn() && !exp.isAssertExp() &&
        !exp.isThrowExp() && !exp.isCallExp())
    {
        auto msg = new StringExp(exp.loc, "Accessed expression of type `noreturn`");
        msg.type = Type.tstring;
        result = new AssertExp(exp.loc, IntegerExp.literal!0, msg);
        result.type = exp.type;
    }

    return result;
}

/******************************
 * Check the tail CallExp is really property function call.
 * Bugs:
 * This doesn't appear to do anything.
 */
private bool checkPropertyCall(Expression e)
{
    e = lastComma(e);

    if (auto ce = e.isCallExp())
    {
        if (ce.f)
        {
            auto tf = ce.f.type.isTypeFunction();
            /* If a forward reference to ce.f, try to resolve it
             */
            if (!tf.deco && ce.f.semanticRun < PASS.semanticdone)
            {
                ce.f.dsymbolSemantic(null);
                tf = ce.f.type.isTypeFunction();
            }
        }
        else if (!ce.e1.type.isFunction_Delegate_PtrToFunction())
            assert(0);
    }
    return false;
}

/******************************
 * Find symbol in accordance with the UFCS name look up rule
 */
private Expression searchUFCS(Scope* sc, UnaExp ue, Identifier ident)
{
    //printf("searchUFCS(ident = %s)\n", ident.toChars());
    Loc loc = ue.loc;

    // TODO: merge with Scope.search.searchScopes()
    Dsymbol searchScopes(int flags)
    {
        Dsymbol s = null;
        for (Scope* scx = sc; scx; scx = scx.enclosing)
        {
            if (!scx.scopesym)
                continue;
            if (scx.scopesym.isModule())
                flags |= SearchUnqualifiedModule;    // tell Module.search() that SearchLocalsOnly is to be obeyed
            s = scx.scopesym.search(loc, ident, flags);
            if (s)
            {
                // overload set contains only module scope symbols.
                if (s.isOverloadSet())
                    break;
                // selective/renamed imports also be picked up
                if (AliasDeclaration ad = s.isAliasDeclaration())
                {
                    if (ad._import)
                        break;
                }
                // See only module scope symbols for UFCS target.
                Dsymbol p = s.toParent2();
                if (p && p.isModule())
                    break;
            }
            s = null;

            // Stop when we hit a module, but keep going if that is not just under the global scope
            if (scx.scopesym.isModule() && !(scx.enclosing && !scx.enclosing.enclosing))
                break;
        }
        return s;
    }

    int flags = 0;
    Dsymbol s;

    if (sc.flags & SCOPE.ignoresymbolvisibility)
        flags |= IgnoreSymbolVisibility;

    // First look in local scopes
    s = searchScopes(flags | SearchLocalsOnly);
    if (!s)
    {
        // Second look in imported modules
        s = searchScopes(flags | SearchImportsOnly);
    }

    if (!s)
        return ue.e1.type.getProperty(sc, loc, ident, 0, ue.e1);

    FuncDeclaration f = s.isFuncDeclaration();
    if (f)
    {
        TemplateDeclaration td = getFuncTemplateDecl(f);
        if (td)
        {
            if (td.overroot)
                td = td.overroot;
            s = td;
        }
    }

    if (auto dti = ue.isDotTemplateInstanceExp())
    {
        auto ti = new TemplateInstance(loc, s.ident, dti.ti.tiargs);
        if (!ti.updateTempDecl(sc, s))
            return ErrorExp.get();
        return new ScopeExp(loc, ti);
    }
    else
    {
        //printf("-searchUFCS() %s\n", s.toChars());
        return new DsymbolExp(loc, s);
    }
}

/******************************
 * Pull out callable entity with UFCS.
 */
private Expression resolveUFCS(Scope* sc, CallExp ce)
{
    Loc loc = ce.loc;
    Expression eleft;
    Expression e;

    if (auto die = ce.e1.isDotIdExp())
    {
        Identifier ident = die.ident;

        Expression ex = die.dotIdSemanticPropX(sc);
        if (ex != die)
        {
            ce.e1 = ex;
            return null;
        }
        eleft = die.e1;

        Type t = eleft.type.toBasetype();
        if (t.ty == Tarray || t.ty == Tsarray || t.ty == Tnull || (t.isTypeBasic() && t.ty != Tvoid))
        {
            /* Built-in types and arrays have no callable properties, so do shortcut.
             * It is necessary in: e.init()
             */
        }
        else if (t.ty == Taarray)
        {
            if (ident == Id.remove)
            {
                /* Transform:
                 *  aa.remove(arg) into delete aa[arg]
                 */
                if (!ce.arguments || ce.arguments.length != 1)
                {
                    ce.error("expected key as argument to `aa.remove()`");
                    return ErrorExp.get();
                }
                if (!eleft.type.isMutable())
                {
                    ce.error("cannot remove key from `%s` associative array `%s`", MODtoChars(t.mod), eleft.toChars());
                    return ErrorExp.get();
                }
                Expression key = (*ce.arguments)[0];
                key = key.expressionSemantic(sc);
                key = resolveProperties(sc, key);

                TypeAArray taa = t.isTypeAArray();
                key = key.implicitCastTo(sc, taa.index);

                if (key.checkValue() || key.checkSharedAccess(sc))
                    return ErrorExp.get();

                semanticTypeInfo(sc, taa.index);

                return new RemoveExp(loc, eleft, key);
            }
        }
        else
        {
            if (Expression ey = die.dotIdSemanticProp(sc, 1))
            {
                if (ey.op == EXP.error)
                    return ey;
                ce.e1 = ey;
                if (isDotOpDispatch(ey))
                {
                    // even opDispatch and UFCS must have valid arguments,
                    // so now that we've seen indication of a problem,
                    // check them for issues.
                    Expressions* originalArguments = Expression.arraySyntaxCopy(ce.arguments);

                    uint errors = global.startGagging();
                    e = ce.expressionSemantic(sc);
                    if (!global.endGagging(errors))
                        return e;

                    if (arrayExpressionSemantic(originalArguments.peekSlice(), sc))
                        return ErrorExp.get();

                    /* fall down to UFCS */
                }
                else
                    return null;
            }
        }

        /* https://issues.dlang.org/show_bug.cgi?id=13953
         *
         * If a struct has an alias this to an associative array
         * and remove is used on a struct instance, we have to
         * check first if there is a remove function that can be called
         * on the struct. If not we must check the alias this.
         *
         * struct A
         * {
         *      string[string] a;
         *      alias a this;
         * }
         *
         * void fun()
         * {
         *      A s;
         *      s.remove("foo");
         * }
         */
        const errors = global.startGagging();
        e = searchUFCS(sc, die, ident);
        // if there were any errors and the identifier was remove
        if (global.endGagging(errors))
        {
            if (ident == Id.remove)
            {
                // check alias this
                Expression alias_e = resolveAliasThis(sc, die.e1, 1);
                if (alias_e && alias_e != die.e1)
                {
                    die.e1 = alias_e;
                    CallExp ce2 = ce.syntaxCopy();
                    ce2.e1 = die;
                    e = ce2.isCallExp().trySemantic(sc);
                    if (e)
                        return e;
                }
            }
            // if alias this did not work out, print the initial errors
            searchUFCS(sc, die, ident);
        }
    }
    else if (auto dti = ce.e1.isDotTemplateInstanceExp())
    {
        if (Expression ey = dti.dotTemplateSemanticProp(sc, 1))
        {
            ce.e1 = ey;
            return null;
        }
        eleft = dti.e1;
        e = searchUFCS(sc, dti, dti.ti.name);
    }
    else
        return null;

    // Rewrite
    ce.e1 = e;
    if (!ce.arguments)
        ce.arguments = new Expressions();
    ce.arguments.shift(eleft);
    if (!ce.names)
        ce.names = new Identifiers();
    ce.names.shift(null);

    return null;
}

/******************************
 * Pull out property with UFCS.
 */
private Expression resolveUFCSProperties(Scope* sc, Expression e1, Expression e2 = null)
{
    Loc loc = e1.loc;
    Expression eleft;
    Expression e;

    if (auto die = e1.isDotIdExp())
    {
        eleft = die.e1;
        e = searchUFCS(sc, die, die.ident);
    }
    else if (auto dti = e1.isDotTemplateInstanceExp())
    {
        eleft = dti.e1;
        e = searchUFCS(sc, dti, dti.ti.name);
    }
    else
        return null;

    if (e is null)
        return null;

    // Rewrite
    if (e2)
    {
        // run semantic without gagging
        e2 = e2.expressionSemantic(sc);

        /* f(e1) = e2
         */
        Expression ex = e.copy();
        auto a1 = new Expressions(1);
        (*a1)[0] = eleft;
        ex = new CallExp(loc, ex, a1);
        auto e1PassSemantic = ex.trySemantic(sc);

        /* f(e1, e2)
         */
        auto a2 = new Expressions(2);
        (*a2)[0] = eleft;
        (*a2)[1] = e2;
        e = new CallExp(loc, e, a2);
        e = e.trySemantic(sc);
        if (!e1PassSemantic && !e)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=20448
             *
             * If both versions have failed to pass semantic,
             * f(e1) = e2 gets priority in error printing
             * because f might be a templated function that
             * failed to instantiate and we have to print
             * the instantiation errors.
             */
            return e1.expressionSemantic(sc);
        }
        else if (ex && !e)
        {
            checkPropertyCall(ex);
            ex = new AssignExp(loc, ex, e2);
            return ex.expressionSemantic(sc);
        }
        else
        {
            // strict setter prints errors if fails
            e = e.expressionSemantic(sc);
        }
        checkPropertyCall(e);
        return e;
    }
    else
    {
        /* f(e1)
         */
        auto arguments = new Expressions(1);
        (*arguments)[0] = eleft;
        e = new CallExp(loc, e, arguments);
        e = e.expressionSemantic(sc);
        checkPropertyCall(e);
        return e.expressionSemantic(sc);
    }
}

/******************************
 * If e1 is a property function (template), resolve it.
 */
Expression resolvePropertiesOnly(Scope* sc, Expression e1)
{
    //printf("e1 = %s %s\n", Token.toChars(e1.op), e1.toChars());

    Expression handleOverloadSet(OverloadSet os)
    {
        assert(os);
        foreach (s; os.a)
        {
            auto fd = s.isFuncDeclaration();
            auto td = s.isTemplateDeclaration();
            if (fd)
            {
                if (fd.type.isTypeFunction().isproperty)
                    return resolveProperties(sc, e1);
            }
            else if (td && td.onemember && (fd = td.onemember.isFuncDeclaration()) !is null)
            {
                if (fd.type.isTypeFunction().isproperty ||
                    (fd.storage_class2 & STC.property) ||
                    (td._scope.stc & STC.property))
                    return resolveProperties(sc, e1);
            }
        }
        return e1;
    }

    Expression handleTemplateDecl(TemplateDeclaration td)
    {
        assert(td);
        if (td.onemember)
        {
            if (auto fd = td.onemember.isFuncDeclaration())
            {
                if (fd.type.isTypeFunction().isproperty ||
                    (fd.storage_class2 & STC.property) ||
                    (td._scope.stc & STC.property))
                    return resolveProperties(sc, e1);
            }
        }
        return e1;
    }

    Expression handleFuncDecl(FuncDeclaration fd)
    {
        assert(fd);
        if (fd.type.isTypeFunction().isproperty)
            return resolveProperties(sc, e1);
        return e1;
    }

    if (auto de = e1.isDotExp())
    {
        if (auto os = de.e2.isOverExp())
            return handleOverloadSet(os.vars);
    }
    else if (auto oe = e1.isOverExp())
        return handleOverloadSet(oe.vars);
    else if (auto dti = e1.isDotTemplateInstanceExp())
    {
        if (dti.ti.tempdecl)
            if (auto td = dti.ti.tempdecl.isTemplateDeclaration())
                return handleTemplateDecl(td);
    }
    else if (auto dte = e1.isDotTemplateExp())
        return handleTemplateDecl(dte.td);
    else if (auto se = e1.isScopeExp())
    {
        Dsymbol s = se.sds;
        TemplateInstance ti = s.isTemplateInstance();
        if (ti && !ti.semanticRun && ti.tempdecl)
            if (auto td = ti.tempdecl.isTemplateDeclaration())
                return handleTemplateDecl(td);
    }
    else if (auto et = e1.isTemplateExp())
        return handleTemplateDecl(et.td);
    else if (e1.isDotVarExp() && e1.type.isTypeFunction())
    {
        DotVarExp dve = e1.isDotVarExp();
        return handleFuncDecl(dve.var.isFuncDeclaration());
    }
    else if (e1.isVarExp() && e1.type && e1.type.isTypeFunction() && (sc.intypeof || !e1.isVarExp().var.needThis()))
        return handleFuncDecl(e1.isVarExp().var.isFuncDeclaration());
    return e1;
}

/****************************************
 * Turn symbol `s` into the expression it represents.
 *
 * Params:
 *      s = symbol to resolve
 *      loc = location of use of `s`
 *      sc = context
 *      hasOverloads = applies if `s` represents a function.
 *          true means it's overloaded and will be resolved later,
 *          false means it's the exact function symbol.
 * Returns:
 *      `s` turned into an expression, `ErrorExp` if an error occurred
 */
Expression symbolToExp(Dsymbol s, const ref Loc loc, Scope *sc, bool hasOverloads)
{
    static if (LOGSEMANTIC)
    {
        printf("DsymbolExp::resolve(%s %s)\n", s.kind(), s.toChars());
    }

Lagain:
    Expression e;

    //printf("DsymbolExp:: %p '%s' is a symbol\n", this, toChars());
    //printf("s = '%s', s.kind = '%s'\n", s.toChars(), s.kind());
    Dsymbol olds = s;
    Declaration d = s.isDeclaration();
    if (d && (d.storage_class & STC.templateparameter))
    {
        s = s.toAlias();
    }
    else
    {
        // functions are checked after overloading
        // templates are checked after matching constraints
        if (!s.isFuncDeclaration() && !s.isTemplateDeclaration())
        {
            s.checkDeprecated(loc, sc);
            if (d)
                d.checkDisabled(loc, sc);
        }

        // https://issues.dlang.org/show_bug.cgi?id=12023
        // if 's' is a tuple variable, the tuple is returned.
        s = s.toAlias();

        //printf("s = '%s', s.kind = '%s', s.needThis() = %p\n", s.toChars(), s.kind(), s.needThis());
        if (s != olds && !s.isFuncDeclaration() && !s.isTemplateDeclaration())
        {
            s.checkDeprecated(loc, sc);
            if (d)
                d.checkDisabled(loc, sc);
        }

        if (auto sd = s.isDeclaration())
        {
            if (sd.isSystem())
            {
                if (sc.setUnsafePreview(global.params.systemVariables, false, loc,
                    "cannot access `@system` variable `%s` in @safe code", sd))
                {
                    return ErrorExp.get();
                }
            }
        }
    }

    if (auto em = s.isEnumMember())
    {
        return em.getVarExp(loc, sc);
    }
    if (auto v = s.isVarDeclaration())
    {
        //printf("Identifier '%s' is a variable, type '%s'\n", s.toChars(), v.type.toChars());
        if (sc.intypeof == 1 && !v.inuse)
            v.dsymbolSemantic(sc);
        if (!v.type ||                  // during variable type inference
            !v.type.deco && v.inuse)    // during variable type semantic
        {
            if (v.inuse)    // variable type depends on the variable itself
                error(loc, "circular reference to %s `%s`", v.kind(), v.toPrettyChars());
            else            // variable type cannot be determined
                error(loc, "forward reference to %s `%s`", v.kind(), v.toPrettyChars());
            return ErrorExp.get();
        }
        if (v.type.ty == Terror)
            return ErrorExp.get();

        if ((v.storage_class & STC.manifest) && v._init)
        {
            if (v.inuse)
            {
                error(loc, "circular initialization of %s `%s`", v.kind(), v.toPrettyChars());
                return ErrorExp.get();
            }
            e = v.expandInitializer(loc);
            v.inuse++;
            e = e.expressionSemantic(sc);
            v.inuse--;
            return e;
        }

        // We need to run semantics to correctly set 'STC.field' if it is a member variable
        // that could be forward referenced. This is needed for 'v.needThis()' to work
        if (v.isThis())
            v.dsymbolSemantic(sc);

        // Change the ancestor lambdas to delegate before hasThis(sc) call.
        if (v.checkNestedReference(sc, loc))
            return ErrorExp.get();

        if (v.needThis() && hasThis(sc))
            e = new DotVarExp(loc, new ThisExp(loc), v);
        else
            e = new VarExp(loc, v);
        e = e.expressionSemantic(sc);
        return e;
    }
    if (auto fld = s.isFuncLiteralDeclaration())
    {
        //printf("'%s' is a function literal\n", fld.toChars());
        e = new FuncExp(loc, fld);
        return e.expressionSemantic(sc);
    }
    if (auto f = s.isFuncDeclaration())
    {
        f = f.toAliasFunc();
        if (!f.functionSemantic())
            return ErrorExp.get();

        if (!hasOverloads && f.checkForwardRef(loc))
            return ErrorExp.get();

        auto fd = s.isFuncDeclaration();
        fd.type = f.type;
        return new VarExp(loc, fd, hasOverloads);
    }
    if (OverDeclaration od = s.isOverDeclaration())
    {
        e = new VarExp(loc, od, true);
        e.type = Type.tvoid;
        return e;
    }
    if (OverloadSet o = s.isOverloadSet())
    {
        //printf("'%s' is an overload set\n", o.toChars());
        return new OverExp(loc, o);
    }

    if (Import imp = s.isImport())
    {
        if (!imp.pkg)
        {
            .error(loc, "forward reference of import `%s`", imp.toChars());
            return ErrorExp.get();
        }
        auto ie = new ScopeExp(loc, imp.pkg);
        return ie.expressionSemantic(sc);
    }
    if (Package pkg = s.isPackage())
    {
        auto ie = new ScopeExp(loc, pkg);
        return ie.expressionSemantic(sc);
    }
    if (Module mod = s.isModule())
    {
        auto ie = new ScopeExp(loc, mod);
        return ie.expressionSemantic(sc);
    }
    if (Nspace ns = s.isNspace())
    {
        auto ie = new ScopeExp(loc, ns);
        return ie.expressionSemantic(sc);
    }

    if (Type t = s.getType())
    {
        return (new TypeExp(loc, t)).expressionSemantic(sc);
    }

    if (TupleDeclaration tup = s.isTupleDeclaration())
    {
        if (tup.needThis() && hasThis(sc))
            e = new DotVarExp(loc, new ThisExp(loc), tup);
        else
            e = new TupleExp(loc, tup);
        e = e.expressionSemantic(sc);
        return e;
    }

    if (TemplateInstance ti = s.isTemplateInstance())
    {
        ti.dsymbolSemantic(sc);
        if (!ti.inst || ti.errors)
            return ErrorExp.get();
        s = ti.toAlias();
        if (!s.isTemplateInstance())
            goto Lagain;
        e = new ScopeExp(loc, ti);
        e = e.expressionSemantic(sc);
        return e;
    }
    if (TemplateDeclaration td = s.isTemplateDeclaration())
    {
        Dsymbol p = td.toParentLocal();
        FuncDeclaration fdthis = hasThis(sc);
        AggregateDeclaration ad = p ? p.isAggregateDeclaration() : null;
        if (fdthis && ad && fdthis.isMemberLocal() == ad && (td._scope.stc & STC.static_) == 0)
        {
            e = new DotTemplateExp(loc, new ThisExp(loc), td);
        }
        else
            e = new TemplateExp(loc, td);
        e = e.expressionSemantic(sc);
        return e;
    }

    .error(loc, "%s `%s` is not a variable", s.kind(), s.toChars());
    return ErrorExp.get();
}

/*************************************************************
 * Given var, get the
 * right `this` pointer if var is in an outer class, but our
 * existing `this` pointer is in an inner class.
 * Params:
 *      loc = location to use for error messages
 *      sc = context
 *      ad = struct or class we need the correct `this` for
 *      e1 = existing `this`
 *      var = the specific member of ad we're accessing
 *      flag = if true, return `null` instead of throwing an error
 * Returns:
 *      Expression representing the `this` for the var
 */
private Expression getRightThis(const ref Loc loc, Scope* sc, AggregateDeclaration ad, Expression e1, Dsymbol var, int flag = 0)
{
    //printf("\ngetRightThis(e1 = %s, ad = %s, var = %s)\n", e1.toChars(), ad.toChars(), var.toChars());
L1:
    Type t = e1.type.toBasetype();
    //printf("e1.type = %s, var.type = %s\n", e1.type.toChars(), var.type.toChars());

    if (e1.op == EXP.objcClassReference)
    {
        // We already have an Objective-C class reference, just use that as 'this'.
        return e1;
    }
    else if (ad && ad.isClassDeclaration && ad.isClassDeclaration.classKind == ClassKind.objc &&
             var.isFuncDeclaration && var.isFuncDeclaration.isStatic &&
             var.isFuncDeclaration.objc.selector)
    {
        return new ObjcClassReferenceExp(e1.loc, ad.isClassDeclaration());
    }

    /* Access of a member which is a template parameter in dual-scope scenario
     * class A { inc(alias m)() { ++m; } } // `m` needs `this` of `B`
     * class B {int m; inc() { new A().inc!m(); } }
     */
    if (e1.op == EXP.this_)
    {
        FuncDeclaration f = hasThis(sc);
        if (f && f.hasDualContext())
        {
            if (f.followInstantiationContext(ad))
            {
                e1 = new VarExp(loc, f.vthis);
                e1 = new PtrExp(loc, e1);
                e1 = new IndexExp(loc, e1, IntegerExp.literal!1);
                e1 = getThisSkipNestedFuncs(loc, sc, f.toParent2(), ad, e1, t, var);
                if (e1.op == EXP.error)
                    return e1;
                goto L1;
            }
        }
    }

    /* If e1 is not the 'this' pointer for ad
     */
    if (ad &&
        !(t.isTypePointer() && t.nextOf().isTypeStruct() && t.nextOf().isTypeStruct().sym == ad) &&
        !(t.isTypeStruct() && t.isTypeStruct().sym == ad))
    {
        ClassDeclaration cd = ad.isClassDeclaration();
        ClassDeclaration tcd = t.isClassHandle();

        /* e1 is the right this if ad is a base class of e1
         */
        if (!cd || !tcd || !(tcd == cd || cd.isBaseOf(tcd, null)))
        {
            /* Only classes can be inner classes with an 'outer'
             * member pointing to the enclosing class instance
             */
            if (tcd && tcd.isNested())
            {
                /* e1 is the 'this' pointer for an inner class: tcd.
                 * Rewrite it as the 'this' pointer for the outer class.
                 */
                auto vthis = tcd.followInstantiationContext(ad) ? tcd.vthis2 : tcd.vthis;
                e1 = new DotVarExp(loc, e1, vthis);
                e1.type = vthis.type;
                e1.type = e1.type.addMod(t.mod);
                // Do not call ensureStaticLinkTo()
                //e1 = e1.semantic(sc);

                // Skip up over nested functions, and get the enclosing
                // class type.
                e1 = getThisSkipNestedFuncs(loc, sc, tcd.toParentP(ad), ad, e1, t, var);
                if (e1.op == EXP.error)
                    return e1;
                goto L1;
            }

            /* Can't find a path from e1 to ad
             */
            if (flag)
                return null;
            e1.error("`this` for `%s` needs to be type `%s` not type `%s`", var.toChars(), ad.toChars(), t.toChars());
            return ErrorExp.get();
        }
    }
    return e1;
}

/*
 * Check whether `outerFunc` and `calledFunc` have the same `this`.
 * If `calledFunc` is the member of a base class of the class that contains
 * `outerFunc` we consider that they have the same this.
 *
 * This function is used to test whether `this` needs to be prepended to
 * a function call or function symbol. For example:
 *
 * struct X
 * {
 *    void gun() {}
 * }
 * struct A
 * {
 *      void fun() {}
 *      void sun()
 *      {
 *          fun();
 *          X.gun();  // error
 *      }
 * }
 *
 * When `fun` is called, `outerfunc` = `sun` and `calledFunc = `fun`.
 * `sun` is a member of `A` and `fun` is also a member of `A`, therefore
 * `this` can be prepended to `fun`. When `gun` is called (it will result
 * in an error, but that is not relevant here), which is a member of `X`,
 * no `this` is needed because the outer function does not have the same
 * `this` as `gun`.
 *
 * Returns:
 *  `true` if outerFunc and calledFunc may use the same `this` pointer.
 * `false` otherwise.
 */
private bool haveSameThis(FuncDeclaration outerFunc, FuncDeclaration calledFunc)
{
    auto thisAd = outerFunc.isMemberLocal();
    if (!thisAd)
        return false;

    auto requiredAd = calledFunc.isMemberLocal();
    if (!requiredAd)
        return false;

    if (thisAd == requiredAd)
        return true;

    // outerfunc is the member of a base class that contains calledFunc,
    // then we consider that they have the same this.
    auto cd = requiredAd.isClassDeclaration();
    if (!cd)
        return false;

    if (cd.isBaseOf2(thisAd.isClassDeclaration()))
        return true;

    // if outerfunc is the member of a nested aggregate, then let
    // getRightThis take care of this.
    if (thisAd.isNested())
        return true;

    return false;
}

/***************************************
 * Pull out any properties.
 */
private Expression resolvePropertiesX(Scope* sc, Expression e1, Expression e2 = null)
{
    //printf("resolvePropertiesX, e1 = %s %s, e2 = %s\n", EXPtoString(e1.op).ptr, e1.toChars(), e2 ? e2.toChars() : null);
    Loc loc = e1.loc;

    OverloadSet os;
    Dsymbol s;
    Objects* tiargs;
    Type tthis;
    if (auto de = e1.isDotExp())
    {
        if (auto oe = de.e2.isOverExp())
        {
            tiargs = null;
            tthis = de.e1.type;
            os = oe.vars;
            goto Los;
        }
    }
    else if (e1.isOverExp())
    {
        tiargs = null;
        tthis = null;
        os = e1.isOverExp().vars;
    Los:
        assert(os);
        FuncDeclaration fd = null;
        if (e2)
        {
            e2 = e2.expressionSemantic(sc);
            if (e2.op == EXP.error)
                return ErrorExp.get();
            e2 = resolveProperties(sc, e2);

            Expressions a;
            a.push(e2);

            for (size_t i = 0; i < os.a.length; i++)
            {
                if (FuncDeclaration f = resolveFuncCall(loc, sc, os.a[i], tiargs, tthis, ArgumentList(&a), FuncResolveFlag.quiet))
                {
                    if (f.errors)
                        return ErrorExp.get();
                    fd = f;
                    assert(fd.type.ty == Tfunction);
                }
            }
            if (fd)
            {
                Expression e = new CallExp(loc, e1, e2);
                return e.expressionSemantic(sc);
            }
        }
        {
            for (size_t i = 0; i < os.a.length; i++)
            {
                if (FuncDeclaration f = resolveFuncCall(loc, sc, os.a[i], tiargs, tthis, ArgumentList(), FuncResolveFlag.quiet))
                {
                    if (f.errors)
                        return ErrorExp.get();
                    fd = f;
                    assert(fd.type.ty == Tfunction);
                    auto tf = fd.type.isTypeFunction();
                    if (!tf.isref && e2)
                    {
                        error(loc, "%s is not an lvalue", e1.toChars());
                        return ErrorExp.get();
                    }
                }
            }
            if (fd)
            {
                Expression e = new CallExp(loc, e1);
                if (e2)
                    e = new AssignExp(loc, e, e2);
                return e.expressionSemantic(sc);
            }
        }
        if (e2)
            goto Leprop;
    }
    else if (auto dti = e1.isDotTemplateInstanceExp())
    {
        if (!dti.findTempDecl(sc))
            goto Leprop;
        if (!dti.ti.semanticTiargs(sc))
            goto Leprop;
        tiargs = dti.ti.tiargs;
        tthis = dti.e1.type;
        if ((os = dti.ti.tempdecl.isOverloadSet()) !is null)
            goto Los;
        if ((s = dti.ti.tempdecl) !is null)
            goto Lfd;
    }
    else if (auto dte = e1.isDotTemplateExp())
    {
        s = dte.td;
        tiargs = null;
        tthis = dte.e1.type;
        goto Lfd;
    }
    else if (auto se = e1.isScopeExp())
    {
        s = se.sds;
        TemplateInstance ti = s.isTemplateInstance();
        if (ti && !ti.semanticRun && ti.tempdecl)
        {
            //assert(ti.needsTypeInference(sc));
            if (!ti.semanticTiargs(sc))
                goto Leprop;
            tiargs = ti.tiargs;
            tthis = null;
            if ((os = ti.tempdecl.isOverloadSet()) !is null)
                goto Los;
            if ((s = ti.tempdecl) !is null)
                goto Lfd;
        }
    }
    else if (auto te = e1.isTemplateExp())
    {
        s = te.td;
        tiargs = null;
        tthis = null;
        goto Lfd;
    }
    else if (e1.isDotVarExp() && e1.type && (e1.type.toBasetype().isTypeFunction() || e1.isDotVarExp().var.isOverDeclaration()))
    {
        DotVarExp dve = e1.isDotVarExp();
        s = dve.var;
        tiargs = null;
        tthis = dve.e1.type;
        goto Lfd;
    }
    else if (sc && sc.flags & SCOPE.Cfile && e1.isVarExp() && !e2)
    {
        // ImportC: do not implicitly call function if no ( ) are present
    }
    else if (e1.isVarExp() && e1.type && (e1.type.toBasetype().isTypeFunction() || e1.isVarExp().var.isOverDeclaration()))
    {
        s = e1.isVarExp().var;
        tiargs = null;
        tthis = null;
    Lfd:
        assert(s);
        if (e2)
        {
            e2 = e2.expressionSemantic(sc);
            if (e2.op == EXP.error)
                return ErrorExp.get();
            e2 = resolveProperties(sc, e2);

            Expressions a;
            a.push(e2);

            FuncDeclaration fd = resolveFuncCall(loc, sc, s, tiargs, tthis, ArgumentList(&a), FuncResolveFlag.quiet);
            if (fd && fd.type)
            {
                if (fd.errors)
                    return ErrorExp.get();
                if (!checkSymbolAccess(sc, fd))
                {
                    // @@@DEPRECATED_2.105@@@
                    // When turning into error, uncomment the return statement
                    TypeFunction tf = fd.type.isTypeFunction();
                    deprecation(loc, "function `%s` of type `%s` is not accessible from module `%s`",
                                fd.toPrettyChars(), tf.toChars, sc._module.toChars);
                    //return ErrorExp.get();
                }
                assert(fd.type.ty == Tfunction);
                Expression e = new CallExp(loc, e1, e2);
                return e.expressionSemantic(sc);
            }
        }
        {
            FuncDeclaration fd = resolveFuncCall(loc, sc, s, tiargs, tthis, ArgumentList(), FuncResolveFlag.quiet);
            if (fd && fd.type)
            {
                if (fd.errors)
                    return ErrorExp.get();
                TypeFunction tf = fd.type.isTypeFunction();
                if (!e2 || tf.isref)
                {
                    if (!checkSymbolAccess(sc, fd))
                    {
                        // @@@DEPRECATED_2.105@@@
                        // When turning into error, uncomment the return statement
                        deprecation(loc, "function `%s` of type `%s` is not accessible from module `%s`",
                                    fd.toPrettyChars(), tf.toChars, sc._module.toChars);
                        //return ErrorExp.get();
                    }
                    Expression e = new CallExp(loc, e1);
                    if (e2)
                        e = new AssignExp(loc, e, e2);
                    return e.expressionSemantic(sc);
                }
            }
        }
        if (FuncDeclaration fd = s.isFuncDeclaration())
        {
            // Keep better diagnostic message for invalid property usage of functions
            assert(fd.type.ty == Tfunction);
            Expression e = new CallExp(loc, e1, e2);
            return e.expressionSemantic(sc);
        }
        if (e2)
            goto Leprop;
    }
    if (auto ve = e1.isVarExp())
    {
        if (auto v = ve.var.isVarDeclaration())
        {
            if (ve.checkPurity(sc, v))
                return ErrorExp.get();
        }
    }
    if (e2)
        return null;

    if (e1.type && !e1.isTypeExp()) // function type is not a property
    {
        /* Look for e1 being a lazy parameter; rewrite as delegate call
         * only if the symbol wasn't already treated as a delegate
         */
        auto ve = e1.isVarExp();
        if (ve && ve.var.storage_class & STC.lazy_ && !ve.delegateWasExtracted)
        {
                Expression e = new CallExp(loc, e1);
                return e.expressionSemantic(sc);
        }
        else if (e1.isDotVarExp())
        {
            // Check for reading overlapped pointer field in @safe code.
            if (checkUnsafeAccess(sc, e1, true, true))
                return ErrorExp.get();
        }
        else if (auto ce = e1.isCallExp())
        {
            // Check for reading overlapped pointer field in @safe code.
            if (checkUnsafeAccess(sc, ce.e1, true, true))
                return ErrorExp.get();
        }
    }

    if (!e1.type)
    {
        error(loc, "cannot resolve type for %s", e1.toChars());
        e1 = ErrorExp.get();
    }
    return e1;

Leprop:
    error(loc, "not a property %s", e1.toChars());
    return ErrorExp.get();
}

extern (C++) Expression resolveProperties(Scope* sc, Expression e)
{
    //printf("resolveProperties(%s)\n", e.toChars());
    e = resolvePropertiesX(sc, e);
    if (e.checkRightThis(sc))
        return ErrorExp.get();
    return e;
}

/****************************************
 * The common type is determined by applying ?: to each pair.
 * Output:
 *      exps[]  properties resolved, implicitly cast to common type, rewritten in place
 * Returns:
 *      The common type, or `null` if an error has occured
 */
private Type arrayExpressionToCommonType(Scope* sc, ref Expressions exps)
{
    /* Still have a problem with:
     *  ubyte[][] = [ cast(ubyte[])"hello", [1]];
     * which works if the array literal is initialized top down with the ubyte[][]
     * type, but fails with this function doing bottom up typing.
     */

    //printf("arrayExpressionToCommonType()\n");
    scope IntegerExp integerexp = IntegerExp.literal!0;
    scope CondExp condexp = new CondExp(Loc.initial, integerexp, null, null);

    Type t0 = null;
    Expression e0 = null;
    bool foundType;

    for (size_t i = 0; i < exps.length; i++)
    {
        Expression e = exps[i];
        if (!e)
            continue;

        e = resolveProperties(sc, e);
        if (!e.type)
        {
            e.error("`%s` has no value", e.toChars());
            t0 = Type.terror;
            continue;
        }
        if (e.op == EXP.type)
        {
            foundType = true; // do not break immediately, there might be more errors
            e.checkValue(); // report an error "type T has no value"
            t0 = Type.terror;
            continue;
        }
        if (e.type.ty == Tvoid)
        {
            // void expressions do not concur to the determination of the common
            // type.
            continue;
        }
        if (checkNonAssignmentArrayOp(e))
        {
            t0 = Type.terror;
            continue;
        }

        e = doCopyOrMove(sc, e);

        if (!foundType && t0 && !t0.equals(e.type))
        {
            /* This applies ?: to merge the types. It's backwards;
             * ?: should call this function to merge types.
             */
            condexp.type = null;
            condexp.e1 = e0;
            condexp.e2 = e;
            condexp.loc = e.loc;
            Expression ex = condexp.expressionSemantic(sc);
            if (ex.op == EXP.error)
                e = ex;
            else
            {
                // Convert to common type
                exps[i] = condexp.e1.castTo(sc, condexp.type);
                e = condexp.e2.castTo(sc, condexp.type);
            }
        }
        e0 = e;
        t0 = e.type;
        if (e.op != EXP.error)
            exps[i] = e;
    }

    // [] is typed as void[]
    if (!t0)
        return Type.tvoid;

    // It's an error, don't do the cast
    if (t0.ty == Terror)
        return null;

    for (size_t i = 0; i < exps.length; i++)
    {
        Expression e = exps[i];
        if (!e)
            continue;

        e = e.implicitCastTo(sc, t0);
        if (e.op == EXP.error)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=13024
             * a workaround for the bug in typeMerge -
             * it should paint e1 and e2 by deduced common type,
             * but doesn't in this particular case.
             */
            return null;
        }
        exps[i] = e;
    }
    return t0;
}

private Expression opAssignToOp(const ref Loc loc, EXP op, Expression e1, Expression e2)
{
    Expression e;
    switch (op)
    {
    case EXP.addAssign:
        e = new AddExp(loc, e1, e2);
        break;

    case EXP.minAssign:
        e = new MinExp(loc, e1, e2);
        break;

    case EXP.mulAssign:
        e = new MulExp(loc, e1, e2);
        break;

    case EXP.divAssign:
        e = new DivExp(loc, e1, e2);
        break;

    case EXP.modAssign:
        e = new ModExp(loc, e1, e2);
        break;

    case EXP.andAssign:
        e = new AndExp(loc, e1, e2);
        break;

    case EXP.orAssign:
        e = new OrExp(loc, e1, e2);
        break;

    case EXP.xorAssign:
        e = new XorExp(loc, e1, e2);
        break;

    case EXP.leftShiftAssign:
        e = new ShlExp(loc, e1, e2);
        break;

    case EXP.rightShiftAssign:
        e = new ShrExp(loc, e1, e2);
        break;

    case EXP.unsignedRightShiftAssign:
        e = new UshrExp(loc, e1, e2);
        break;

    default:
        assert(0);
    }
    return e;
}

/*********************
 * Rewrite:
 *    array.length op= e2
 */
private Expression rewriteOpAssign(BinExp exp)
{
    ArrayLengthExp ale = exp.e1.isArrayLengthExp();
    if (ale.e1.isVarExp())
    {
        // array.length = array.length op e2
        Expression e = opAssignToOp(exp.loc, exp.op, ale, exp.e2);
        e = new AssignExp(exp.loc, ale.syntaxCopy(), e);
        return e;
    }
    else
    {
        // (ref tmp = array;), tmp.length = tmp.length op e2
        auto tmp = copyToTemp(STC.ref_, "__arraylength", ale.e1);
        Expression e1 = new ArrayLengthExp(ale.loc, new VarExp(ale.loc, tmp));
        Expression elvalue = e1.syntaxCopy();
        Expression e = opAssignToOp(exp.loc, exp.op, e1, exp.e2);
        e = new AssignExp(exp.loc, elvalue, e);
        e = new CommaExp(exp.loc, new DeclarationExp(ale.loc, tmp), e);
        return e;
    }
}

/****************************************
 * Preprocess arguments to function.
 *
 * Tuples in argumentList get expanded, properties resolved, rewritten in place
 *
 * Params:
 *     sc           =  scope
 *     argumentList =  arguments to function
 *     reportErrors =  whether or not to report errors here. Some callers are not
 *                      checking actual function params, so they'll do their own error reporting
 * Returns:
 *     `true` when a semantic error occurred
 */
private bool preFunctionParameters(Scope* sc, ArgumentList argumentList, const bool reportErrors = true)
{
    Expressions* exps = argumentList.arguments;
    bool err = false;
    if (exps)
    {
        expandTuples(exps, argumentList.names);

        for (size_t i = 0; i < exps.length; i++)
        {
            Expression arg = (*exps)[i];
            arg = resolveProperties(sc, arg);
            arg = arg.arrayFuncConv(sc);
            if (arg.op == EXP.type)
            {
                // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
                arg = resolveAliasThis(sc, arg);

                if (arg.op == EXP.type)
                {
                    if (reportErrors)
                    {
                        arg.error("cannot pass type `%s` as a function argument", arg.toChars());
                        arg = ErrorExp.get();
                    }
                    err = true;
                }
            }
            else if (arg.type.toBasetype().ty == Tfunction)
            {
                if (reportErrors)
                {
                    arg.error("cannot pass function `%s` as a function argument", arg.toChars());
                    arg = ErrorExp.get();
                }
                err = true;
            }
            else if (checkNonAssignmentArrayOp(arg))
            {
                arg = ErrorExp.get();
                err = true;
            }
            (*exps)[i] = arg;
        }
    }
    return err;
}

/********************************************
 * Issue an error if default construction is disabled for type t.
 * Default construction is required for arrays and 'out' parameters.
 * Returns:
 *      true    an error was issued
 */
private bool checkDefCtor(Loc loc, Type t)
{
    if (auto ts = t.baseElemOf().isTypeStruct())
    {
        StructDeclaration sd = ts.sym;
        if (sd.noDefaultCtor)
        {
            sd.error(loc, "default construction is disabled");
            return true;
        }
    }
    return false;
}

/****************************************
 * Now that we know the exact type of the function we're calling,
 * the arguments[] need to be adjusted:
 *      1. implicitly convert argument to the corresponding parameter type
 *      2. add default arguments for any missing arguments
 *      3. do default promotions on arguments corresponding to ...
 *      4. add hidden _arguments[] argument
 *      5. call copy constructor for struct value arguments
 * Params:
 *      loc       = location of function call
 *      sc        = context
 *      tf        = type of the function
 *      ethis     = `this` argument, `null` if none or not known
 *      tthis     = type of `this` argument, `null` if no `this` argument
 *      argumentsList = array of actual arguments to function call
 *      fd        = the function being called, `null` if called indirectly
 *      prettype  = set to return type of function
 *      peprefix  = set to expression to execute before `arguments[]` are evaluated, `null` if none
 * Returns:
 *      true    errors happened
 */
private bool functionParameters(const ref Loc loc, Scope* sc,
    TypeFunction tf, Expression ethis, Type tthis, ArgumentList argumentList, FuncDeclaration fd,
    Type* prettype, Expression* peprefix)
{
    Expressions* arguments = argumentList.arguments;
    //printf("functionParameters() %s\n", fd ? fd.toChars() : "");
    assert(arguments);
    assert(fd || tf.next);
    const size_t nparams = tf.parameterList.length;
    const olderrors = global.errors;
    bool err = false;
    Expression eprefix = null;
    *peprefix = null;

    if (argumentList.names)
    {
        const(char)* msg = null;
        auto resolvedArgs = tf.resolveNamedArgs(argumentList, &msg);
        if (!resolvedArgs)
        {
            // while errors are usually already caught by `tf.callMatch`,
            // this can happen when calling `typeof(freefunc)`
            if (msg)
                error(loc, "%s", msg);
            return true;
        }
        // note: the argument list should be mutated with named arguments / default arguments,
        // so we can't simply change the pointer like `arguments = resolvedArgs;`
        arguments.setDim(0);
        arguments.pushSlice((*resolvedArgs)[]);
    }
    size_t nargs = arguments ? arguments.length : 0;

    if (nargs > nparams && tf.parameterList.varargs == VarArg.none)
    {
        error(loc, "expected %llu arguments, not %llu for non-variadic function type `%s`", cast(ulong)nparams, cast(ulong)nargs, tf.toChars());
        return true;
    }

    // If inferring return type, and semantic3() needs to be run if not already run
    if (!tf.next && fd.inferRetType)
    {
        fd.functionSemantic();
    }
    else if (fd && fd.parent)
    {
        TemplateInstance ti = fd.parent.isTemplateInstance();
        if (ti && ti.tempdecl)
        {
            fd.functionSemantic3();
        }
    }

    /* If calling a pragma(inline, true) function,
     * set flag to later scan for inlines.
     */
    if (fd && fd.inlining == PINLINE.always)
    {
        if (sc._module)
            sc._module.hasAlwaysInlines = true;
        if (sc.func)
            sc.func.hasAlwaysInlines = true;
    }

    const isCtorCall = fd && fd.needThis() && fd.isCtorDeclaration();

    const size_t n = (nargs > nparams) ? nargs : nparams; // n = max(nargs, nparams)

    /* If the function return type has wildcards in it, we'll need to figure out the actual type
     * based on the actual argument types.
     * Start with the `this` argument, later on merge into wildmatch the mod bits of the rest
     * of the arguments.
     */
    MOD wildmatch = (tthis && !isCtorCall) ? tthis.Type.deduceWild(tf, false) : 0;

    bool done = false;
    foreach (const i; 0 .. n)
    {
        Expression arg = (i < nargs) ? (*arguments)[i] : null;

        if (i < nparams)
        {
            bool errorArgs()
            {
                error(loc, "expected %llu function arguments, not %llu", cast(ulong)nparams, cast(ulong)nargs);
                return true;
            }

            Parameter p = tf.parameterList[i];

            if (!arg)
            {
                if (!p.defaultArg)
                {
                    if (tf.parameterList.varargs == VarArg.typesafe && i + 1 == nparams)
                        goto L2;
                    return errorArgs();
                }
                arg = p.defaultArg;
                if (!arg.type)
                    arg = arg.expressionSemantic(sc);
                arg = inlineCopy(arg, sc);
                // __FILE__, __LINE__, __MODULE__, __FUNCTION__, and __PRETTY_FUNCTION__
                arg = arg.resolveLoc(loc, sc);
                if (i >= nargs)
                {
                    arguments.push(arg);
                    nargs++;
                }
                else
                    (*arguments)[i] = arg;
            }
            else
            {
                if (isDefaultInitOp(arg.op))
                {
                    arg = arg.resolveLoc(loc, sc);
                    (*arguments)[i] = arg;
                }
            }


            if (tf.parameterList.varargs == VarArg.typesafe && i + 1 == nparams) // https://dlang.org/spec/function.html#variadic
            {
                //printf("\t\tvarargs == 2, p.type = '%s'\n", p.type.toChars());
                {
                    MATCH m;
                    if ((m = arg.implicitConvTo(p.type)) > MATCH.nomatch)
                    {
                        if (p.type.nextOf() && arg.implicitConvTo(p.type.nextOf()) >= m)
                            goto L2;
                        else if (nargs != nparams)
                            return errorArgs();
                        goto L1;
                    }
                }
            L2:
                Type tb = p.type.toBasetype();
                switch (tb.ty)
                {
                case Tsarray:
                case Tarray:
                    {
                        /* Create a static array variable v of type arg.type:
                         *  T[dim] __arrayArg = [ arguments[i], ..., arguments[nargs-1] ];
                         *
                         * The array literal in the initializer of the hidden variable
                         * is now optimized.
                         * https://issues.dlang.org/show_bug.cgi?id=2356
                         */
                        Type tbn = (cast(TypeArray)tb).next;    // array element type
                        Type tret = p.isLazyArray();

                        auto elements = new Expressions(nargs - i);
                        foreach (u; 0 .. elements.length)
                        {
                            Expression a = (*arguments)[i + u];
                            if (tret && a.implicitConvTo(tret))
                            {
                                // p is a lazy array of delegates, tret is return type of the delegates
                                a = a.implicitCastTo(sc, tret)
                                     .optimize(WANTvalue)
                                     .toDelegate(tret, sc);
                            }
                            else
                                a = a.implicitCastTo(sc, tbn);
                            a = a.addDtorHook(sc);
                            (*elements)[u] = a;
                        }
                        // https://issues.dlang.org/show_bug.cgi?id=14395
                        // Convert to a static array literal, or its slice.
                        arg = new ArrayLiteralExp(loc, tbn.sarrayOf(nargs - i), elements);
                        if (tb.ty == Tarray)
                        {
                            arg = new SliceExp(loc, arg, null, null);
                            arg.type = p.type;
                        }
                        break;
                    }
                case Tclass:
                    {
                        /* Set arg to be:
                         *      new Tclass(arg0, arg1, ..., argn)
                         */
                        auto args = new Expressions(nargs - i);
                        foreach (u; i .. nargs)
                            (*args)[u - i] = (*arguments)[u];
                        arg = new NewExp(loc, null, p.type, args);
                        break;
                    }
                default:
                    if (!arg)
                    {
                        error(loc, "not enough arguments");
                        return true;
                    }
                    break;
                }
                arg = arg.expressionSemantic(sc);
                //printf("\targ = '%s'\n", arg.toChars());
                arguments.setDim(i + 1);
                (*arguments)[i] = arg;
                nargs = i + 1;
                done = true;
            }

        L1:
            if (!(p.isLazy() && p.type.ty == Tvoid))
            {
                if (ubyte wm = arg.type.deduceWild(p.type, p.isReference()))
                {
                    wildmatch = wildmatch ? MODmerge(wildmatch, wm) : wm;
                    //printf("[%d] p = %s, a = %s, wm = %d, wildmatch = %d\n", i, p.type.toChars(), arg.type.toChars(), wm, wildmatch);
                }
            }
        }
        if (done)
            break;
    }
    if ((wildmatch == MODFlags.mutable || wildmatch == MODFlags.immutable_) &&
        tf.next && tf.next.hasWild() &&
        (tf.isref || !tf.next.implicitConvTo(tf.next.immutableOf())))
    {
        bool errorInout(MOD wildmatch)
        {
            const(char)* s = wildmatch == MODFlags.mutable ? "mutable" : MODtoChars(wildmatch);
            error(loc, "modify `inout` to `%s` is not allowed inside `inout` function", s);
            return true;
        }

        if (fd)
        {
            /* If the called function may return the reference to
             * outer inout data, it should be rejected.
             *
             * void foo(ref inout(int) x) {
             *   ref inout(int) bar(inout(int)) { return x; }
             *   struct S {
             *      ref inout(int) bar() inout { return x; }
             *      ref inout(int) baz(alias a)() inout { return x; }
             *   }
             *   bar(int.init) = 1;  // bad!
             *   S().bar() = 1;      // bad!
             * }
             * void test() {
             *   int a;
             *   auto s = foo(a);
             *   s.baz!a() = 1;      // bad!
             * }
             *
             */
            bool checkEnclosingWild(Dsymbol s)
            {
                bool checkWild(Dsymbol s)
                {
                    if (!s)
                        return false;
                    if (auto ad = s.isAggregateDeclaration())
                    {
                        if (ad.isNested())
                            return checkEnclosingWild(s);
                    }
                    else if (auto ff = s.isFuncDeclaration())
                    {
                        if (ff.type.isTypeFunction().iswild)
                            return errorInout(wildmatch);

                        if (ff.isNested() || ff.isThis())
                            return checkEnclosingWild(s);
                    }
                    return false;
                }

                Dsymbol ctx0 = s.toParent2();
                Dsymbol ctx1 = s.toParentLocal();
                if (checkWild(ctx0))
                    return true;
                if (ctx0 != ctx1)
                    return checkWild(ctx1);
                return false;
            }
            if ((fd.isThis() || fd.isNested()) && checkEnclosingWild(fd))
                return true;
        }
        else if (tf.isWild())
            return errorInout(wildmatch);
    }

    Expression firstArg = null;
    final switch (returnParamDest(tf, tthis))
    {
        case ReturnParamDest.returnVal:
            break;
        case ReturnParamDest.firstArg:
            firstArg = nargs > 0 ? (*arguments)[0] : null;
            break;
        case ReturnParamDest.this_:
            firstArg = ethis;
            break;
    }

    assert(nargs >= nparams);
    foreach (const i, arg; (*arguments)[0 .. nargs])
    {
        assert(arg);
        if (i < nparams)
        {
            Parameter p = tf.parameterList[i];
            Type targ = arg.type;               // keep original type for isCopyable() because alias this
                                                // resolution may hide an uncopyable type

            if (!(p.isLazy() && p.type.ty == Tvoid))
            {
                Type tprm = p.type.hasWild()
                    ? p.type.substWildTo(wildmatch)
                    : p.type;

                const hasCopyCtor = arg.type.isTypeStruct() && arg.type.isTypeStruct().sym.hasCopyCtor;
                const typesMatch = arg.type.mutableOf().unSharedOf().equals(tprm.mutableOf().unSharedOf());
                if (!((hasCopyCtor && typesMatch) || tprm.equals(arg.type)))
                {
                    //printf("arg.type = %s, p.type = %s\n", arg.type.toChars(), p.type.toChars());
                    arg = arg.implicitCastTo(sc, tprm);
                    arg = arg.optimize(WANTvalue, p.isReference());
                }
            }

            // Support passing rvalue to `in` parameters
            if ((p.storageClass & (STC.in_ | STC.ref_)) == (STC.in_ | STC.ref_))
            {
                if (!arg.isLvalue())
                {
                    auto v = copyToTemp(STC.exptemp, "__rvalue", arg);
                    Expression ev = new DeclarationExp(arg.loc, v);
                    ev = new CommaExp(arg.loc, ev, new VarExp(arg.loc, v));
                    arg = ev.expressionSemantic(sc);
                }
                arg = arg.toLvalue(sc, arg);

                // Look for mutable misaligned pointer, etc., in @safe mode
                err |= checkUnsafeAccess(sc, arg, false, true);
            }
            else if (p.storageClass & STC.ref_)
            {
                if (global.params.rvalueRefParam == FeatureState.enabled &&
                    !arg.isLvalue() &&
                    targ.isCopyable())
                {   /* allow rvalues to be passed to ref parameters by copying
                     * them to a temp, then pass the temp as the argument
                     */
                    auto v = copyToTemp(0, "__rvalue", arg);
                    Expression ev = new DeclarationExp(arg.loc, v);
                    ev = new CommaExp(arg.loc, ev, new VarExp(arg.loc, v));
                    arg = ev.expressionSemantic(sc);
                }
                arg = arg.toLvalue(sc, arg);

                // Look for mutable misaligned pointer, etc., in @safe mode
                err |= checkUnsafeAccess(sc, arg, false, true);
            }
            else if (p.storageClass & STC.out_)
            {
                Type t = arg.type;
                if (!t.isMutable() || !t.isAssignable()) // check blit assignable
                {
                    arg.error("cannot modify struct `%s` with immutable members", arg.toChars());
                    err = true;
                }
                else
                {
                    // Look for misaligned pointer, etc., in @safe mode
                    err |= checkUnsafeAccess(sc, arg, false, true);
                    err |= checkDefCtor(arg.loc, t); // t must be default constructible
                }
                arg = arg.toLvalue(sc, arg);
            }
            else if (p.isLazy())
            {
                // Convert lazy argument to a delegate
                auto t = (p.type.ty == Tvoid) ? p.type : arg.type;
                arg = toDelegate(arg, t, sc);
            }
            //printf("arg: %s\n", arg.toChars());
            //printf("type: %s\n", arg.type.toChars());
            //printf("param: %s\n", p.toChars());

            const pStc = tf.parameterStorageClass(tthis, p);

            if (firstArg && (pStc & STC.return_))
            {
                /* Argument value can be assigned to firstArg.
                 * Check arg to see if it matters.
                 */
                err |= checkParamArgumentReturn(sc, firstArg, arg, p, false);
            }
            // Allow 'lazy' to imply 'scope' - lazy parameters can be passed along
            // as lazy parameters to the next function, but that isn't escaping.
            else if (!(pStc & STC.lazy_))
            {
                /* Argument value can escape from the called function.
                 * Check arg to see if it matters.
                 */
                VarDeclaration vPar = fd ? (fd.parameters ? (*fd.parameters)[i] : null) : null;
                err |= checkParamArgumentEscape(sc, fd, p.ident, vPar, cast(STC) pStc, arg, false, false);
            }

            // Turning heap allocations into stack allocations is dangerous without dip1000, since `scope` inference
            // may be unreliable when scope violations only manifest as deprecation warnings.
            // However, existing `@nogc` code may rely on it, so still do it when the parameter is explicitly marked `scope`
            const explicitScope = p.isLazy() ||
                ((p.storageClass & STC.scope_) && !(p.storageClass & STC.scopeinferred));
            if ((pStc & (STC.scope_ | STC.lazy_)) &&
                ((global.params.useDIP1000 == FeatureState.enabled) || explicitScope) &&
                !(pStc & STC.return_))
            {
                /* Argument value cannot escape from the called function.
                 */
                Expression a = arg;
                if (auto ce = a.isCastExp())
                    a = ce.e1;

                ArrayLiteralExp ale;
                if (p.type.toBasetype().ty == Tarray &&
                    (ale = a.isArrayLiteralExp()) !is null && ale.elements && ale.elements.length > 0)
                {
                    // allocate the array literal as temporary static array on the stack
                    ale.type = ale.type.nextOf().sarrayOf(ale.elements.length);
                    auto tmp = copyToTemp(0, "__arrayliteral_on_stack", ale);
                    auto declareTmp = new DeclarationExp(ale.loc, tmp);
                    auto castToSlice = new CastExp(ale.loc, new VarExp(ale.loc, tmp),
                        p.type.substWildTo(MODFlags.mutable));
                    arg = CommaExp.combine(declareTmp, castToSlice);
                    arg = arg.expressionSemantic(sc);
                }
                else if (auto fe = a.isFuncExp())
                {
                    /* Function literals can only appear once, so if this
                     * appearance was scoped, there cannot be any others.
                     */
                    fe.fd.tookAddressOf = 0;
                }
                else if (auto de = a.isDelegateExp())
                {
                    /* For passing a delegate to a scoped parameter,
                     * this doesn't count as taking the address of it.
                     * We only worry about 'escaping' references to the function.
                     */
                    if (auto ve = de.e1.isVarExp())
                    {
                        if (auto f = ve.var.isFuncDeclaration())
                        {
                            if (f.tookAddressOf)
                                --f.tookAddressOf;
                            //printf("--tookAddressOf = %d\n", f.tookAddressOf);
                        }
                    }
                }
            }
            if (!p.isReference())
                err |= arg.checkSharedAccess(sc);

            arg = arg.optimize(WANTvalue, p.isReference());
        }
        else
        {
            // These will be the trailing ... arguments
            // If not D linkage, do promotions
            if (tf.linkage != LINK.d)
            {
                // Promote bytes, words, etc., to ints
                arg = integralPromotions(arg, sc);

                // Promote floats to doubles
                switch (arg.type.ty)
                {
                case Tfloat32:
                    arg = arg.castTo(sc, Type.tfloat64);
                    break;

                case Timaginary32:
                    arg = arg.castTo(sc, Type.timaginary64);
                    break;

                default:
                    break;
                }
                if (tf.parameterList.varargs == VarArg.variadic)
                {
                    const(char)* p = tf.linkage == LINK.c ? "extern(C)" : "extern(C++)";
                    if (arg.type.ty == Tarray)
                    {
                        arg.error("cannot pass dynamic arrays to `%s` vararg functions", p);
                        err = true;
                    }
                    if (arg.type.ty == Tsarray)
                    {
                        arg.error("cannot pass static arrays to `%s` vararg functions", p);
                        err = true;
                    }
                }
            }

            // Do not allow types that need destructors or copy constructors.
            if (arg.type.needsDestruction())
            {
                arg.error("cannot pass types that need destruction as variadic arguments");
                err = true;
            }
            if (arg.type.needsCopyOrPostblit())
            {
                arg.error("cannot pass types with postblits or copy constructors as variadic arguments");
                err = true;
            }

            // Convert static arrays to dynamic arrays
            // BUG: I don't think this is right for D2
            Type tb = arg.type.toBasetype();
            if (auto ts = tb.isTypeSArray())
            {
                Type ta = ts.next.arrayOf();
                if (ts.size(arg.loc) == 0)
                    arg = new NullExp(arg.loc, ta);
                else
                    arg = arg.castTo(sc, ta);
            }
            if (tb.ty == Tstruct)
            {
                //arg = callCpCtor(sc, arg);
            }
            // Give error for overloaded function addresses
            if (auto se = arg.isSymOffExp())
            {
                if (se.hasOverloads && !se.var.isFuncDeclaration().isUnique())
                {
                    arg.error("function `%s` is overloaded", arg.toChars());
                    err = true;
                }
            }
            err |= arg.checkValue();
            err |= arg.checkSharedAccess(sc);
            arg = arg.optimize(WANTvalue);
        }
        (*arguments)[i] = arg;
    }

    /* If calling C scanf(), printf(), or any variants, check the format string against the arguments
     */
    const isVa_list = tf.parameterList.varargs == VarArg.none;
    if (fd && fd.printf)
    {
        if (auto se = (*arguments)[nparams - 1 - isVa_list].isStringExp())
        {
            checkPrintfFormat(se.loc, se.peekString(), (*arguments)[nparams .. nargs], isVa_list);
        }
    }
    else if (fd && fd.scanf)
    {
        if (auto se = (*arguments)[nparams - 1 - isVa_list].isStringExp())
        {
            checkScanfFormat(se.loc, se.peekString(), (*arguments)[nparams .. nargs], isVa_list);
        }
    }
    else
    {
        // TODO: not checking the "v" functions yet (for those, check format string only, not args)
    }

    /* Remaining problems:
     * 1. order of evaluation - some function push L-to-R, others R-to-L. Until we resolve what array assignment does (which is
     *    implemented by calling a function) we'll defer this for now.
     * 2. value structs (or static arrays of them) that need to be copy constructed
     * 3. value structs (or static arrays of them) that have destructors, and subsequent arguments that may throw before the
     *    function gets called.
     * 4. value structs need to be destructed after the function call for platforms where the caller destroys the arguments.
     * 2, 3 and 4 are handled by doing the argument construction in 'eprefix' so that if a later argument throws, they are cleaned
     * up properly. Pushing arguments on the stack then cannot fail.
     */
     {
        /* TODO: tackle problem 1)
         */
        const bool leftToRight = true; // TODO: Any cases that need rightToLeft?
        if (!leftToRight)
            assert(nargs == nparams); // no variadics for RTL order, as they would probably be evaluated LTR and so add complexity

        /* Does Problem (4) apply?
         */
        const bool callerDestroysArgs = !target.isCalleeDestroyingArgs(tf);

        const ptrdiff_t start = (leftToRight ? 0 : cast(ptrdiff_t)nargs - 1);
        const ptrdiff_t end   = (leftToRight ? cast(ptrdiff_t)nargs : -1);
        const ptrdiff_t step  = (leftToRight ? 1 : -1);

        /* Compute indices of last throwing argument and first arg needing destruction.
         * Used to not set up destructors unless an arg needs destruction on a throw
         * in a later argument.
         */
        ptrdiff_t lastthrow = -1;   // last argument that may throw
        ptrdiff_t firstdtor = -1;   // first argument that needs destruction
        ptrdiff_t lastdtor  = -1;   // last argument that needs destruction
        for (ptrdiff_t i = start; i != end; i += step)
        {
            Expression arg = (*arguments)[i];
            if (canThrow(arg, sc.func, false))
                lastthrow = i;
            if (arg.type.needsDestruction())
            {
                Parameter p = (i >= nparams ? null : tf.parameterList[i]);
                if (!(p && (p.isLazy() || p.isReference())))
                {
                    if (firstdtor == -1)
                        firstdtor = i;
                    lastdtor = i;
                }
            }
        }

        /* Do we need 'eprefix' for problems 3 or 4?
         */
        const bool needsPrefix = callerDestroysArgs
            ? firstdtor >= 0 // true if any argument needs destruction
            : firstdtor >= 0 && lastthrow >= 0 &&
              (lastthrow - firstdtor) * step > 0; // last throw after first destruction
        const ptrdiff_t lastPrefix = callerDestroysArgs
            ? lastdtor   // up to last argument requiring destruction
            : lastthrow; // up to last potentially throwing argument

        /* Problem 3: initialize 'eprefix' by declaring the gate
         */
        VarDeclaration gate;
        if (needsPrefix && !callerDestroysArgs)
        {
            // eprefix => bool __gate [= false]
            Identifier idtmp = Identifier.generateId("__gate");
            gate = new VarDeclaration(loc, Type.tbool, idtmp, null);
            gate.storage_class |= STC.temp | STC.ctfe | STC.volatile_;
            gate.dsymbolSemantic(sc);

            auto ae = new DeclarationExp(loc, gate);
            eprefix = ae.expressionSemantic(sc);
        }

        for (ptrdiff_t i = start; i != end; i += step)
        {
            Expression arg = (*arguments)[i];
            //printf("arg[%d]: %s\n", cast(int)i, arg.toChars());

            Parameter parameter = (i >= nparams ? null : tf.parameterList[i]);
            const bool isRef = parameter && parameter.isReference();
            const bool isLazy = parameter && parameter.isLazy();

            /* Skip lazy parameters
             */
            if (isLazy)
                continue;

            /* Do we have 'eprefix' and aren't past 'lastPrefix' yet?
             * Then declare a temporary variable for this arg and append that declaration
             * to 'eprefix', which will implicitly take care of potential problem 2) for
             * this arg.
             * 'eprefix' will therefore finally contain all args up to and including 'lastPrefix',
             * excluding all lazy parameters.
             */
            if (needsPrefix && (lastPrefix - i) * step >= 0)
            {
                const bool needsDtor = !isRef && arg.type.needsDestruction() &&
                                       // Problem 3: last throwing arg doesn't require dtor patching
                                       (callerDestroysArgs || i != lastPrefix);

                /* Declare temporary 'auto __pfx = arg' (needsDtor) or 'auto __pfy = arg' (!needsDtor)
                 */
                auto tmp = copyToTemp(
                    (parameter ? parameter.storageClass : tf.parameterList.stc) & (STC.scope_),
                    needsDtor ? "__pfx" : "__pfy",
                    !isRef ? arg : arg.addressOf());
                tmp.dsymbolSemantic(sc);

                if (callerDestroysArgs)
                {
                    /* Problem 4: Normal temporary, destructed after the call
                     */
                    if (needsDtor)
                        tmp.isArgDtorVar = true;   // mark it so that the backend passes it by ref to the function being called
                }
                else
                {
                    /* Problem 3: Modify the destructor so it only runs if gate==false,
                     * i.e., only if there was a throw while constructing the args
                     */
                    if (!needsDtor)
                    {
                        if (tmp.edtor)
                        {
                            assert(i == lastPrefix);
                            tmp.edtor = null;
                        }
                    }
                    else
                    {
                        // edtor => (__gate || edtor)
                        assert(tmp.edtor);
                        Expression e = tmp.edtor;
                        e = new LogicalExp(e.loc, EXP.orOr, new VarExp(e.loc, gate), e);
                        tmp.edtor = e.expressionSemantic(sc);
                        //printf("edtor: %s\n", tmp.edtor.toChars());
                    }
                }

                // eprefix => (eprefix, auto __pfx/y = arg)
                auto ae = new DeclarationExp(loc, tmp);
                eprefix = Expression.combine(eprefix, ae.expressionSemantic(sc));

                // arg => __pfx/y
                arg = new VarExp(loc, tmp);
                arg = arg.expressionSemantic(sc);
                if (isRef)
                {
                    arg = new PtrExp(loc, arg);
                    arg = arg.expressionSemantic(sc);
                }

                /* Problem 3: Last throwing arg?
                 * Then finalize eprefix => (eprefix, gate = true), i.e., disable the
                 * dtors right after constructing the last throwing arg.
                 * From now on, the callee will take care of destructing the args because
                 * the args are implicitly moved into function parameters.
                 */
                if (!callerDestroysArgs && i == lastPrefix)
                {
                    auto e = new AssignExp(gate.loc, new VarExp(gate.loc, gate), IntegerExp.createBool(true));
                    eprefix = Expression.combine(eprefix, e.expressionSemantic(sc));
                }
            }
            else // not part of 'eprefix'
            {
                /* Handle problem 2) by calling the copy constructor for value structs
                 * (or static arrays of them) if appropriate.
                 */
                Type tv = arg.type.baseElemOf();
                if (!isRef && tv.ty == Tstruct)
                    arg = doCopyOrMove(sc, arg, parameter ? parameter.type : null);
            }

            (*arguments)[i] = arg;
        }
    }
    //if (eprefix) printf("eprefix: %s\n", eprefix.toChars());

    /* Test compliance with DIP1021 Argument Ownership and Function Calls
     */
    if (global.params.useDIP1021 && (tf.trust == TRUST.safe || tf.trust == TRUST.default_) ||
        tf.islive)
        err |= checkMutableArguments(sc, fd, tf, ethis, arguments, false);

    // If D linkage and variadic, add _arguments[] as first argument
    if (tf.isDstyleVariadic())
    {
        assert(arguments.length >= nparams);

        auto args = new Parameters(arguments.length - nparams);
        for (size_t i = 0; i < arguments.length - nparams; i++)
        {
            auto arg = new Parameter(STC.in_, (*arguments)[nparams + i].type, null, null, null);
            (*args)[i] = arg;
        }
        auto tup = new TypeTuple(args);
        Expression e = (new TypeidExp(loc, tup)).expressionSemantic(sc);
        arguments.insert(0, e);
    }

    /* Determine function return type: tret
     */
    Type tret = tf.next;
    if (isCtorCall)
    {
        //printf("[%s] fd = %s %s, %d %d %d\n", loc.toChars(), fd.toChars(), fd.type.toChars(),
        //    wildmatch, tf.isWild(), fd.isReturnIsolated());
        if (!tthis)
        {
            assert(sc.intypeof || global.errors);
            tthis = fd.isThis().type.addMod(fd.type.mod);
        }
        if (tf.isWild() && !fd.isReturnIsolated())
        {
            if (wildmatch)
                tret = tret.substWildTo(wildmatch);
            int offset;
            if (!tret.implicitConvTo(tthis) && !(MODimplicitConv(tret.mod, tthis.mod) && tret.isBaseOf(tthis, &offset) && offset == 0))
            {
                const(char)* s1 = tret.isNaked() ? " mutable" : tret.modToChars();
                const(char)* s2 = tthis.isNaked() ? " mutable" : tthis.modToChars();
                .error(loc, "`inout` constructor `%s` creates%s object, not%s", fd.toPrettyChars(), s1, s2);
                err = true;
            }
        }
        tret = tthis;
    }
    else if (wildmatch && tret)
    {
        /* Adjust function return type based on wildmatch
         */
        //printf("wildmatch = x%x, tret = %s\n", wildmatch, tret.toChars());
        tret = tret.substWildTo(wildmatch);
    }

    *prettype = tret;
    *peprefix = eprefix;
    return (err || olderrors != global.errors);
}

/**
 * Determines whether a symbol represents a module or package
 * (Used as a helper for is(type == module) and is(type == package))
 *
 * Params:
 *  sym = the symbol to be checked
 *
 * Returns:
 *  the symbol which `sym` represents (or `null` if it doesn't represent a `Package`)
 */
Package resolveIsPackage(Dsymbol sym)
{
    Package pkg;
    if (Import imp = sym.isImport())
    {
        if (imp.pkg is null)
        {
            .error(sym.loc, "internal compiler error: unable to process forward-referenced import `%s`",
                    imp.toChars());
            assert(0);
        }
        pkg = imp.pkg;
    }
    else if (auto mod = sym.isModule())
        pkg = mod.isPackageFile ? mod.pkg : sym.isPackage();
    else
        pkg = sym.isPackage();
    if (pkg)
        pkg.resolvePKGunknown();
    return pkg;
}


private extern (C++) final class ExpressionSemanticVisitor : Visitor
{
    alias visit = Visitor.visit;

    Scope* sc;
    Expression result;

    this(Scope* sc) scope
    {
        this.sc = sc;
    }

    private void setError()
    {
        result = ErrorExp.get();
    }

    /**************************
     * Semantically analyze Expression.
     * Determine types, fold constants, etc.
     */
    override void visit(Expression e)
    {
        static if (LOGSEMANTIC)
        {
            printf("Expression::semantic() %s\n", e.toChars());
        }
        if (e.type)
            e.type = e.type.typeSemantic(e.loc, sc);
        else
            e.type = Type.tvoid;
        result = e;
    }

    override void visit(IntegerExp e)
    {
        assert(e.type);
        if (e.type.ty == Terror)
            return setError();

        assert(e.type.deco);
        e.setInteger(e.getInteger());
        result = e;
    }

    override void visit(RealExp e)
    {
        if (!e.type)
            e.type = Type.tfloat64;
        else
            e.type = e.type.typeSemantic(e.loc, sc);
        result = e;
    }

    override void visit(ComplexExp e)
    {
        if (!e.type)
            e.type = Type.tcomplex80;
        else
            e.type = e.type.typeSemantic(e.loc, sc);
        result = e;
    }

    override void visit(IdentifierExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("IdentifierExp::semantic('%s')\n", exp.ident.toChars());
        }
        if (exp.type) // This is used as the dummy expression
        {
            result = exp;
            return;
        }

        Dsymbol scopesym;
        Dsymbol s = sc.search(exp.loc, exp.ident, &scopesym);
        if (s)
        {
            if (s.errors)
                return setError();

            Expression e;

            /* See if the symbol was a member of an enclosing 'with'
             */
            WithScopeSymbol withsym = scopesym.isWithScopeSymbol();
            if (withsym && withsym.withstate.wthis && symbolIsVisible(sc, s))
            {
                /* Disallow shadowing
                 */
                // First find the scope of the with
                Scope* scwith = sc;
                while (scwith.scopesym != scopesym)
                {
                    scwith = scwith.enclosing;
                    assert(scwith);
                }
                // Look at enclosing scopes for symbols with the same name,
                // in the same function
                for (Scope* scx = scwith; scx && scx.func == scwith.func; scx = scx.enclosing)
                {
                    Dsymbol s2;
                    if (scx.scopesym && scx.scopesym.symtab && (s2 = scx.scopesym.symtab.lookup(s.ident)) !is null && s != s2)
                    {
                        exp.error("with symbol `%s` is shadowing local symbol `%s`", s.toPrettyChars(), s2.toPrettyChars());
                        return setError();
                    }
                }
                s = s.toAlias();

                // Same as wthis.ident
                //  TODO: DotIdExp.semantic will find 'ident' from 'wthis' again.
                //  The redudancy should be removed.
                e = new VarExp(exp.loc, withsym.withstate.wthis);
                e = new DotIdExp(exp.loc, e, exp.ident);
                e = e.expressionSemantic(sc);
            }
            else
            {
                if (withsym)
                {
                    if (withsym.withstate.exp.type.ty != Tvoid)
                    {
                        // 'with (exp)' is a type expression
                        // or 's' is not visible there (for error message)
                        e = new TypeExp(exp.loc, withsym.withstate.exp.type);
                    }
                    else
                    {
                        // 'with (exp)' is a Package/Module
                        e = withsym.withstate.exp;
                    }
                    e = new DotIdExp(exp.loc, e, exp.ident);
                    result = e.expressionSemantic(sc);
                    return;
                }

                /* If f is really a function template,
                 * then replace f with the function template declaration.
                 */
                FuncDeclaration f = s.isFuncDeclaration();
                if (f)
                {
                    TemplateDeclaration td = getFuncTemplateDecl(f);
                    if (td)
                    {
                        if (td.overroot) // if not start of overloaded list of TemplateDeclaration's
                            td = td.overroot; // then get the start
                        e = new TemplateExp(exp.loc, td, f);
                        e = e.expressionSemantic(sc);
                        result = e;
                        return;
                    }
                }

                if (global.params.fixAliasThis)
                {
                    ExpressionDsymbol expDsym = scopesym.isExpressionDsymbol();
                    if (expDsym)
                    {
                        //printf("expDsym = %s\n", expDsym.exp.toChars());
                        result = expDsym.exp.expressionSemantic(sc);
                        return;
                    }
                }
                // Haven't done overload resolution yet, so pass 1
                e = symbolToExp(s, exp.loc, sc, true);
            }
            result = e;
            return;
        }

        if (!global.params.fixAliasThis && hasThis(sc))
        {
            for (AggregateDeclaration ad = sc.getStructClassScope(); ad;)
            {
                if (ad.aliasthis)
                {
                    Expression e;
                    e = new ThisExp(exp.loc);
                    e = new DotIdExp(exp.loc, e, ad.aliasthis.ident);
                    e = new DotIdExp(exp.loc, e, exp.ident);
                    e = e.trySemantic(sc);
                    if (e)
                    {
                        result = e;
                        return;
                    }
                }

                auto cd = ad.isClassDeclaration();
                if (cd && cd.baseClass && cd.baseClass != ClassDeclaration.object)
                {
                    ad = cd.baseClass;
                    continue;
                }
                break;
            }
        }

        if (exp.ident == Id.ctfe)
        {
            if (sc.flags & SCOPE.ctfe)
            {
                exp.error("variable `__ctfe` cannot be read at compile time");
                return setError();
            }

            // Create the magic __ctfe bool variable
            auto vd = new VarDeclaration(exp.loc, Type.tbool, Id.ctfe, null);
            vd.storage_class |= STC.temp;
            vd.semanticRun = PASS.semanticdone;
            Expression e = new VarExp(exp.loc, vd);
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }

        // If we've reached this point and are inside a with() scope then we may
        // try one last attempt by checking whether the 'wthis' object supports
        // dynamic dispatching via opDispatch.
        // This is done by rewriting this expression as wthis.ident.
        // The innermost with() scope of the hierarchy to satisfy the condition
        // above wins.
        // https://issues.dlang.org/show_bug.cgi?id=6400
        for (Scope* sc2 = sc; sc2; sc2 = sc2.enclosing)
        {
            if (!sc2.scopesym)
                continue;

            if (auto ss = sc2.scopesym.isWithScopeSymbol())
            {
                if (ss.withstate.wthis)
                {
                    Expression e;
                    e = new VarExp(exp.loc, ss.withstate.wthis);
                    e = new DotIdExp(exp.loc, e, exp.ident);
                    e = e.trySemantic(sc);
                    if (e)
                    {
                        result = e;
                        return;
                    }
                }
                // Try Type.opDispatch (so the static version)
                else if (ss.withstate.exp && ss.withstate.exp.op == EXP.type)
                {
                    if (Type t = ss.withstate.exp.isTypeExp().type)
                    {
                        Expression e;
                        e = new TypeExp(exp.loc, t);
                        e = new DotIdExp(exp.loc, e, exp.ident);
                        e = e.trySemantic(sc);
                        if (e)
                        {
                            result = e;
                            return;
                        }
                    }
                }
            }
        }

        /* Look for what user might have meant
         */
        if (const n = importHint(exp.ident.toString()))
            exp.error("`%s` is not defined, perhaps `import %.*s;` is needed?", exp.ident.toChars(), cast(int)n.length, n.ptr);
        else if (auto s2 = sc.search_correct(exp.ident))
            exp.error("undefined identifier `%s`, did you mean %s `%s`?", exp.ident.toChars(), s2.kind(), s2.toChars());
        else if (const p = Scope.search_correct_C(exp.ident))
            exp.error("undefined identifier `%s`, did you mean `%s`?", exp.ident.toChars(), p);
        else if (exp.ident == Id.dollar)
            exp.error("undefined identifier `$`");
        else
            exp.error("undefined identifier `%s`", exp.ident.toChars());

        result = ErrorExp.get();
    }

    override void visit(DsymbolExp e)
    {
        result = symbolToExp(e.s, e.loc, sc, e.hasOverloads);
    }

    override void visit(ThisExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("ThisExp::semantic()\n");
        }
        if (e.type)
        {
            result = e;
            return;
        }

        FuncDeclaration fd = hasThis(sc); // fd is the uplevel function with the 'this' variable
        AggregateDeclaration ad;

        /* Special case for typeof(this) and typeof(super) since both
         * should work even if they are not inside a non-static member function
         */
        if (!fd && sc.intypeof == 1)
        {
            // Find enclosing struct or class
            for (Dsymbol s = sc.getStructClassScope(); 1; s = s.parent)
            {
                if (!s)
                {
                    e.error("`%s` is not in a class or struct scope", e.toChars());
                    goto Lerr;
                }
                ClassDeclaration cd = s.isClassDeclaration();
                if (cd)
                {
                    e.type = cd.type;
                    result = e;
                    return;
                }
                StructDeclaration sd = s.isStructDeclaration();
                if (sd)
                {
                    e.type = sd.type;
                    result = e;
                    return;
                }
            }
        }
        if (!fd)
            goto Lerr;

        assert(fd.vthis);
        e.var = fd.vthis;
        assert(e.var.parent);
        ad = fd.isMemberLocal();
        if (!ad)
            ad = fd.isMember2();
        assert(ad);
        e.type = ad.type.addMod(e.var.type.mod);

        if (e.var.checkNestedReference(sc, e.loc))
            return setError();

        result = e;
        return;

    Lerr:
        e.error("`this` is only defined in non-static member functions, not `%s`", sc.parent.toChars());
        result = ErrorExp.get();
    }

    override void visit(SuperExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("SuperExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        FuncDeclaration fd = hasThis(sc);
        ClassDeclaration cd;
        Dsymbol s;

        /* Special case for typeof(this) and typeof(super) since both
         * should work even if they are not inside a non-static member function
         */
        if (!fd && sc.intypeof == 1)
        {
            // Find enclosing class
            for (s = sc.getStructClassScope(); 1; s = s.parent)
            {
                if (!s)
                {
                    e.error("`%s` is not in a class scope", e.toChars());
                    goto Lerr;
                }
                cd = s.isClassDeclaration();
                if (cd)
                {
                    cd = cd.baseClass;
                    if (!cd)
                    {
                        e.error("class `%s` has no `super`", s.toChars());
                        goto Lerr;
                    }
                    e.type = cd.type;
                    result = e;
                    return;
                }
            }
        }
        if (!fd)
            goto Lerr;

        e.var = fd.vthis;
        assert(e.var && e.var.parent);

        s = fd.toParentDecl();
        if (s.isTemplateDeclaration()) // allow inside template constraint
            s = s.toParent();
        assert(s);
        cd = s.isClassDeclaration();
        //printf("parent is %s %s\n", fd.toParent().kind(), fd.toParent().toChars());
        if (!cd)
            goto Lerr;
        if (!cd.baseClass)
        {
            e.error("no base class for `%s`", cd.toChars());
            e.type = cd.type.addMod(e.var.type.mod);
        }
        else
        {
            e.type = cd.baseClass.type;
            e.type = e.type.castMod(e.var.type.mod);
        }

        if (e.var.checkNestedReference(sc, e.loc))
            return setError();

        result = e;
        return;

    Lerr:
        e.error("`super` is only allowed in non-static class member functions");
        result = ErrorExp.get();
    }

    override void visit(NullExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("NullExp::semantic('%s')\n", e.toChars());
        }
        // NULL is the same as (void *)0
        if (e.type)
        {
            result = e;
            return;
        }
        e.type = Type.tnull;
        result = e;
    }

    override void visit(StringExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("StringExp::semantic() %s\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        OutBuffer buffer;
        size_t newlen = 0;
        size_t u;
        dchar c;

        switch (e.postfix)
        {
        case 'd':
            for (u = 0; u < e.len;)
            {
                if (const p = utf_decodeChar(e.peekString(), u, c))
                {
                    e.error("%.*s", cast(int)p.length, p.ptr);
                    return setError();
                }
                else
                {
                    buffer.write4(c);
                    newlen++;
                }
            }
            buffer.write4(0);
            e.setData(buffer.extractData(), newlen, 4);
            if (sc && sc.flags & SCOPE.Cfile)
                e.type = Type.tuns32.sarrayOf(e.len + 1);
            else
                e.type = Type.tdchar.immutableOf().arrayOf();
            e.committed = 1;
            break;

        case 'w':
            for (u = 0; u < e.len;)
            {
                if (const p = utf_decodeChar(e.peekString(), u, c))
                {
                    e.error("%.*s", cast(int)p.length, p.ptr);
                    return setError();
                }
                else
                {
                    buffer.writeUTF16(c);
                    newlen++;
                    if (c >= 0x10000)
                        newlen++;
                }
            }
            buffer.writeUTF16(0);
            e.setData(buffer.extractData(), newlen, 2);
            if (sc && sc.flags & SCOPE.Cfile)
                e.type = Type.tuns16.sarrayOf(e.len + 1);
            else
                e.type = Type.twchar.immutableOf().arrayOf();
            e.committed = 1;
            break;

        case 'c':
            e.committed = 1;
            goto default;

        default:
            if (sc && sc.flags & SCOPE.Cfile)
                e.type = Type.tchar.sarrayOf(e.len + 1);
            else
                e.type = Type.tchar.immutableOf().arrayOf();
            break;
        }
        e.type = e.type.typeSemantic(e.loc, sc);
        //type = type.immutableOf();
        //printf("type = %s\n", type.toChars());

        result = e;
    }

    override void visit(TupleExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("+TupleExp::semantic(%s)\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (exp.e0)
            exp.e0 = exp.e0.expressionSemantic(sc);

        // Run semantic() on each argument
        bool err = false;
        for (size_t i = 0; i < exp.exps.length; i++)
        {
            Expression e = (*exp.exps)[i];
            e = e.expressionSemantic(sc);
            if (!e.type)
            {
                exp.error("`%s` has no value", e.toChars());
                err = true;
            }
            else if (e.op == EXP.error)
                err = true;
            else
                (*exp.exps)[i] = e;
        }
        if (err)
            return setError();

        expandTuples(exp.exps);

        exp.type = new TypeTuple(exp.exps);
        exp.type = exp.type.typeSemantic(exp.loc, sc);
        //printf("-TupleExp::semantic(%s)\n", toChars());
        result = exp;
    }

    override void visit(ArrayLiteralExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("ArrayLiteralExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        /* Perhaps an empty array literal [ ] should be rewritten as null?
         */

        if (e.basis)
            e.basis = e.basis.expressionSemantic(sc);
        if (arrayExpressionSemantic(e.elements.peekSlice(), sc) || (e.basis && e.basis.op == EXP.error))
            return setError();

        expandTuples(e.elements);

        if (e.basis)
            e.elements.push(e.basis);
        Type t0 = arrayExpressionToCommonType(sc, *e.elements);
        if (e.basis)
            e.basis = e.elements.pop();
        if (t0 is null)
            return setError();

        e.type = t0.arrayOf();
        e.type = e.type.typeSemantic(e.loc, sc);

        /* Disallow array literals of type void being used.
         */
        if (e.elements.length > 0 && t0.ty == Tvoid)
        {
            e.error("`%s` of type `%s` has no value", e.toChars(), e.type.toChars());
            return setError();
        }

        if (global.params.useTypeInfo && Type.dtypeinfo)
            semanticTypeInfo(sc, e.type);

        result = e;
    }

    override void visit(AssocArrayLiteralExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("AssocArrayLiteralExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        // Run semantic() on each element
        bool err_keys = arrayExpressionSemantic(e.keys.peekSlice(), sc);
        bool err_vals = arrayExpressionSemantic(e.values.peekSlice(), sc);
        if (err_keys || err_vals)
            return setError();

        expandTuples(e.keys);
        expandTuples(e.values);
        if (e.keys.length != e.values.length)
        {
            e.error("number of keys is %llu, must match number of values %llu",
                        cast(ulong) e.keys.length, cast(ulong) e.values.length);
            return setError();
        }

        Type tkey = arrayExpressionToCommonType(sc, *e.keys);
        Type tvalue = arrayExpressionToCommonType(sc, *e.values);
        if (tkey is null || tvalue is null)
            return setError();

        e.type = new TypeAArray(tvalue, tkey);
        e.type = e.type.typeSemantic(e.loc, sc);

        semanticTypeInfo(sc, e.type);

        if (checkAssocArrayLiteralEscape(sc, e, false))
            return setError();

        result = e;
    }

    override void visit(StructLiteralExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("StructLiteralExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        e.sd.size(e.loc);
        if (e.sd.sizeok != Sizeok.done)
            return setError();

        // run semantic() on each element
        if (arrayExpressionSemantic(e.elements.peekSlice(), sc))
            return setError();

        expandTuples(e.elements);

        /* Fit elements[] to the corresponding type of field[].
         */
        if (!e.sd.fit(e.loc, sc, e.elements, e.stype))
            return setError();

        /* Fill out remainder of elements[] with default initializers for fields[]
         */
        if (!e.sd.fill(e.loc, *e.elements, false))
        {
            /* An error in the initializer needs to be recorded as an error
             * in the enclosing function or template, since the initializer
             * will be part of the stuct declaration.
             */
            global.increaseErrorCount();
            return setError();
        }

        if (checkFrameAccess(e.loc, sc, e.sd, e.elements.length))
            return setError();

        e.type = e.stype ? e.stype : e.sd.type;
        result = e;
    }

    override void visit(CompoundLiteralExp cle)
    {
        static if (LOGSEMANTIC)
        {
            printf("CompoundLiteralExp::semantic('%s')\n", cle.toChars());
        }
        Type t = cle.type.typeSemantic(cle.loc, sc);
        auto init = initializerSemantic(cle.initializer, sc, t, INITnointerpret);
        auto e = initializerToExpression(init, t, (sc.flags & SCOPE.Cfile) != 0);
        if (!e)
        {
            error(cle.loc, "cannot convert initializer `%s` to expression", init.toChars());
            return setError();
        }
        result = e;
        return;
    }

    override void visit(TypeExp exp)
    {
        if (exp.type.ty == Terror)
            return setError();

        //printf("TypeExp::semantic(%s)\n", exp.type.toChars());
        Expression e;
        Type t;
        Dsymbol s;

        dmd.typesem.resolve(exp.type, exp.loc, sc, e, t, s, true);
        if (e)
        {
            // `(Type)` is actually `(var)` so if `(var)` is a member requiring `this`
            // then rewrite as `(this.var)` in case it would be followed by a DotVar
            // to fix https://issues.dlang.org/show_bug.cgi?id=9490
            VarExp ve = e.isVarExp();
            if (ve && ve.var && exp.parens && !ve.var.isStatic() && !(sc.stc & STC.static_) &&
                sc.func && sc.func.needThis && ve.var.isMember2())
            {
                // printf("apply fix for issue 9490: add `this.` to `%s`...\n", e.toChars());
                e = new DotVarExp(exp.loc, new ThisExp(exp.loc), ve.var, false);
            }
            //printf("e = %s %s\n", Token.toChars(e.op), e.toChars());
            e = e.expressionSemantic(sc);
        }
        else if (t)
        {
            //printf("t = %d %s\n", t.ty, t.toChars());
            exp.type = t.typeSemantic(exp.loc, sc);
            e = exp;
        }
        else if (s)
        {
            //printf("s = %s %s\n", s.kind(), s.toChars());
            e = symbolToExp(s, exp.loc, sc, true);
        }
        else
            assert(0);

        exp.type.checkComplexTransition(exp.loc, sc);

        result = e;
    }

    override void visit(ScopeExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("+ScopeExp::semantic(%p '%s')\n", exp, exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        ScopeDsymbol sds2 = exp.sds;
        TemplateInstance ti = sds2.isTemplateInstance();
        while (ti)
        {
            WithScopeSymbol withsym;
            if (!ti.findTempDecl(sc, &withsym) || !ti.semanticTiargs(sc))
                return setError();
            if (withsym && withsym.withstate.wthis)
            {
                Expression e = new VarExp(exp.loc, withsym.withstate.wthis);
                e = new DotTemplateInstanceExp(exp.loc, e, ti);
                result = e.expressionSemantic(sc);
                return;
            }
            if (ti.needsTypeInference(sc))
            {
                if (TemplateDeclaration td = ti.tempdecl.isTemplateDeclaration())
                {
                    Dsymbol p = td.toParentLocal();
                    FuncDeclaration fdthis = hasThis(sc);
                    AggregateDeclaration ad = p ? p.isAggregateDeclaration() : null;
                    if (fdthis && ad && fdthis.isMemberLocal() == ad && (td._scope.stc & STC.static_) == 0)
                    {
                        Expression e = new DotTemplateInstanceExp(exp.loc, new ThisExp(exp.loc), ti);
                        result = e.expressionSemantic(sc);
                        return;
                    }
                }
                else if (OverloadSet os = ti.tempdecl.isOverloadSet())
                {
                    FuncDeclaration fdthis = hasThis(sc);
                    AggregateDeclaration ad = os.parent.isAggregateDeclaration();
                    if (fdthis && ad && fdthis.isMemberLocal() == ad)
                    {
                        Expression e = new DotTemplateInstanceExp(exp.loc, new ThisExp(exp.loc), ti);
                        result = e.expressionSemantic(sc);
                        return;
                    }
                }
                // ti is an instance which requires IFTI.
                exp.sds = ti;
                exp.type = Type.tvoid;
                result = exp;
                return;
            }
            ti.dsymbolSemantic(sc);
            if (!ti.inst || ti.errors)
                return setError();

            Dsymbol s = ti.toAlias();
            if (s == ti)
            {
                exp.sds = ti;
                exp.type = Type.tvoid;
                result = exp;
                return;
            }
            sds2 = s.isScopeDsymbol();
            if (sds2)
            {
                ti = sds2.isTemplateInstance();
                //printf("+ sds2 = %s, '%s'\n", sds2.kind(), sds2.toChars());
                continue;
            }

            if (auto v = s.isVarDeclaration())
            {
                if (!v.type)
                {
                    exp.error("forward reference of %s `%s`", v.kind(), v.toChars());
                    return setError();
                }
                if ((v.storage_class & STC.manifest) && v._init)
                {
                    /* When an instance that will be converted to a constant exists,
                     * the instance representation "foo!tiargs" is treated like a
                     * variable name, and its recursive appearance check (note that
                     * it's equivalent with a recursive instantiation of foo) is done
                     * separately from the circular initialization check for the
                     * eponymous enum variable declaration.
                     *
                     *  template foo(T) {
                     *    enum bool foo = foo;    // recursive definition check (v.inuse)
                     *  }
                     *  template bar(T) {
                     *    enum bool bar = bar!T;  // recursive instantiation check (ti.inuse)
                     *  }
                     */
                    if (ti.inuse)
                    {
                        exp.error("recursive expansion of %s `%s`", ti.kind(), ti.toPrettyChars());
                        return setError();
                    }
                    v.checkDeprecated(exp.loc, sc);
                    auto e = v.expandInitializer(exp.loc);
                    ti.inuse++;
                    e = e.expressionSemantic(sc);
                    ti.inuse--;
                    result = e;
                    return;
                }
            }

            //printf("s = %s, '%s'\n", s.kind(), s.toChars());
            auto e = symbolToExp(s, exp.loc, sc, true);
            //printf("-1ScopeExp::semantic()\n");
            result = e;
            return;
        }

        //printf("sds2 = %s, '%s'\n", sds2.kind(), sds2.toChars());
        //printf("\tparent = '%s'\n", sds2.parent.toChars());
        sds2.dsymbolSemantic(sc);

        // (Aggregate|Enum)Declaration
        if (auto t = sds2.getType())
        {
            result = (new TypeExp(exp.loc, t)).expressionSemantic(sc);
            return;
        }

        if (auto td = sds2.isTemplateDeclaration())
        {
            result = (new TemplateExp(exp.loc, td)).expressionSemantic(sc);
            return;
        }

        exp.sds = sds2;
        exp.type = Type.tvoid;
        //printf("-2ScopeExp::semantic() %s\n", toChars());
        result = exp;
    }

    override void visit(NewExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("NewExp::semantic() %s\n", exp.toChars());
            if (exp.thisexp)
                printf("\tthisexp = %s\n", exp.thisexp.toChars());
            printf("\tnewtype: %s\n", exp.newtype.toChars());
        }
        if (exp.type) // if semantic() already run
        {
            result = exp;
            return;
        }

        //for error messages if the argument in [] is not convertible to size_t
        const originalNewtype = exp.newtype;

        // https://issues.dlang.org/show_bug.cgi?id=11581
        // With the syntax `new T[edim]` or `thisexp.new T[edim]`,
        // T should be analyzed first and edim should go into arguments iff it's
        // not a tuple.
        Expression edim = null;
        if (!exp.arguments && exp.newtype.isTypeSArray())
        {
            auto ts = exp.newtype.isTypeSArray();
            // check `new Value[Key]`
            ts.dim = ts.dim.expressionSemantic(sc);
            if (ts.dim.op == EXP.type)
            {
                exp.newtype = new TypeAArray(ts.next, ts.dim.isTypeExp().type);
            }
            else
            {
                edim = ts.dim;
                exp.newtype = ts.next;
            }
        }

        ClassDeclaration cdthis = null;
        if (exp.thisexp)
        {
            exp.thisexp = exp.thisexp.expressionSemantic(sc);
            if (exp.thisexp.op == EXP.error)
                return setError();

            cdthis = exp.thisexp.type.isClassHandle();
            if (!cdthis)
            {
                exp.error("`this` for nested class must be a class type, not `%s`", exp.thisexp.type.toChars());
                return setError();
            }

            sc = sc.push(cdthis);
            exp.type = exp.newtype.typeSemantic(exp.loc, sc);
            sc = sc.pop();
        }
        else
        {
            exp.type = exp.newtype.typeSemantic(exp.loc, sc);
        }
        if (exp.type.ty == Terror)
            return setError();

        if (edim)
        {
            if (exp.type.toBasetype().ty == Ttuple)
            {
                // --> new T[edim]
                exp.type = new TypeSArray(exp.type, edim);
                exp.type = exp.type.typeSemantic(exp.loc, sc);
                if (exp.type.ty == Terror)
                    return setError();
            }
            else
            {
                // --> new T[](edim)
                exp.arguments = new Expressions();
                exp.arguments.push(edim);
                exp.type = exp.type.arrayOf();
            }
        }

        exp.newtype = exp.type; // in case type gets cast to something else
        Type tb = exp.type.toBasetype();
        //printf("tb: %s, deco = %s\n", tb.toChars(), tb.deco);
        if (arrayExpressionSemantic(exp.arguments.peekSlice(), sc))
        {
            return setError();
        }
        if (preFunctionParameters(sc, exp.argumentList))
        {
            return setError();
        }

        if (exp.thisexp && tb.ty != Tclass)
        {
            exp.error("`.new` is only for allocating nested classes, not `%s`", tb.toChars());
            return setError();
        }

        const size_t nargs = exp.arguments ? exp.arguments.length : 0;
        Expression newprefix = null;

        if (auto tc = tb.isTypeClass())
        {
            auto cd = tc.sym;
            if (cd.errors)
                return setError();
            cd.size(exp.loc);
            if (cd.sizeok != Sizeok.done)
                return setError();
            if (!cd.ctor)
                cd.ctor = cd.searchCtor();
            if (cd.noDefaultCtor && !nargs && !cd.defaultCtor)
            {
                exp.error("default construction is disabled for type `%s`", cd.type.toChars());
                return setError();
            }

            if (cd.isInterfaceDeclaration())
            {
                exp.error("cannot create instance of interface `%s`", cd.toChars());
                return setError();
            }

            if (cd.isAbstract())
            {
                exp.error("cannot create instance of abstract class `%s`", cd.toChars());
                for (size_t i = 0; i < cd.vtbl.length; i++)
                {
                    FuncDeclaration fd = cd.vtbl[i].isFuncDeclaration();
                    if (fd && fd.isAbstract())
                    {
                        errorSupplemental(exp.loc, "function `%s` is not implemented",
                            fd.toFullSignature());
                    }
                }
                return setError();
            }
            // checkDeprecated() is already done in newtype.typeSemantic().

            if (cd.isNested())
            {
                /* We need a 'this' pointer for the nested class.
                 * Ensure we have the right one.
                 */
                Dsymbol s = cd.toParentLocal();

                //printf("cd isNested, parent = %s '%s'\n", s.kind(), s.toPrettyChars());
                if (auto cdn = s.isClassDeclaration())
                {
                    if (!cdthis)
                    {
                        void noReferenceToOuterClass()
                        {
                            if (cd.isAnonymous)
                                exp.error("cannot construct anonymous nested class because no implicit `this` reference to outer class is available");
                            else
                                exp.error("cannot construct nested class `%s` because no implicit `this` reference to outer class `%s` is available",
                                    cd.toChars(), cdn.toChars());
                            return setError();
                        }

                        if (!sc.hasThis)
                            return noReferenceToOuterClass();

                        // Supply an implicit 'this' and try again
                        exp.thisexp = new ThisExp(exp.loc);
                        for (Dsymbol sp = sc.parent; 1; sp = sp.toParentLocal())
                        {
                            if (!sp)
                                return noReferenceToOuterClass();
                            ClassDeclaration cdp = sp.isClassDeclaration();
                            if (!cdp)
                                continue;
                            if (cdp == cdn || cdn.isBaseOf(cdp, null))
                                break;
                            // Add a '.outer' and try again
                            exp.thisexp = new DotIdExp(exp.loc, exp.thisexp, Id.outer);
                        }

                        exp.thisexp = exp.thisexp.expressionSemantic(sc);
                        if (exp.thisexp.op == EXP.error)
                            return setError();
                        cdthis = exp.thisexp.type.isClassHandle();
                    }
                    if (cdthis != cdn && !cdn.isBaseOf(cdthis, null))
                    {
                        //printf("cdthis = %s\n", cdthis.toChars());
                        exp.error("`this` for nested class must be of type `%s`, not `%s`",
                            cdn.toChars(), exp.thisexp.type.toChars());
                        return setError();
                    }
                    if (!MODimplicitConv(exp.thisexp.type.mod, exp.newtype.mod))
                    {
                        exp.error("nested type `%s` should have the same or weaker constancy as enclosing type `%s`",
                            exp.newtype.toChars(), exp.thisexp.type.toChars());
                        return setError();
                    }
                }
                else if (exp.thisexp)
                {
                    exp.error("`.new` is only for allocating nested classes");
                    return setError();
                }
                else if (auto fdn = s.isFuncDeclaration())
                {
                    // make sure the parent context fdn of cd is reachable from sc
                    if (!ensureStaticLinkTo(sc.parent, fdn))
                    {
                        exp.error("outer function context of `%s` is needed to `new` nested class `%s`",
                            fdn.toPrettyChars(), cd.toPrettyChars());
                        return setError();
                    }
                }
                else
                    assert(0);
            }
            else if (exp.thisexp)
            {
                exp.error("`.new` is only for allocating nested classes");
                return setError();
            }

            if (cd.vthis2)
            {
                if (AggregateDeclaration ad2 = cd.isMember2())
                {
                    Expression te = new ThisExp(exp.loc).expressionSemantic(sc);
                    if (te.op != EXP.error)
                        te = getRightThis(exp.loc, sc, ad2, te, cd);
                    if (te.op == EXP.error)
                    {
                        exp.error("need `this` of type `%s` needed to `new` nested class `%s`", ad2.toChars(), cd.toChars());
                        return setError();
                    }
                }
            }

            if (cd.disableNew && !exp.onstack)
            {
                exp.error("cannot allocate `class %s` with `new` because it is annotated with `@disable new()`",
                          originalNewtype.toChars());
                return setError();
            }

            if (cd.ctor)
            {
                FuncDeclaration f = resolveFuncCall(exp.loc, sc, cd.ctor, null, tb, exp.argumentList, FuncResolveFlag.standard);
                if (!f || f.errors)
                    return setError();

                checkFunctionAttributes(exp, sc, f);
                checkAccess(cd, exp.loc, sc, f);

                TypeFunction tf = f.type.isTypeFunction();
                if (!exp.arguments)
                    exp.arguments = new Expressions();
                if (functionParameters(exp.loc, sc, tf, null, exp.type, exp.argumentList, f, &exp.type, &exp.argprefix))
                    return setError();

                exp.member = f.isCtorDeclaration();
                assert(exp.member);
            }
            else
            {
                if (nargs)
                {
                    exp.error("no constructor for `%s`", cd.toChars());
                    return setError();
                }

                // https://issues.dlang.org/show_bug.cgi?id=19941
                // Run semantic on all field initializers to resolve any forward
                // references. This is the same as done for structs in sd.fill().
                for (ClassDeclaration c = cd; c; c = c.baseClass)
                {
                    foreach (v; c.fields)
                    {
                        if (v.inuse || v._scope is null || v._init is null ||
                            v._init.isVoidInitializer())
                            continue;
                        v.inuse++;
                        v._init = v._init.initializerSemantic(v._scope, v.type, INITinterpret);
                        v.inuse--;
                    }
                }
            }

            // When using `@nogc` exception handling, lower `throw new E(args)` to
            // `throw (__tmp = _d_newThrowable!E(), __tmp.__ctor(args), __tmp)`.
            if (global.params.ehnogc && exp.thrownew &&
                !cd.isCOMclass() && !cd.isCPPclass())
            {
                assert(cd.ctor);

                Expression id = new IdentifierExp(exp.loc, Id.empty);
                id = new DotIdExp(exp.loc, id, Id.object);

                auto tiargs = new Objects();
                tiargs.push(exp.newtype);
                id = new DotTemplateInstanceExp(exp.loc, id, Id._d_newThrowable, tiargs);
                id = new CallExp(exp.loc, id).expressionSemantic(sc);

                Expression idVal;
                Expression tmp = extractSideEffect(sc, "__tmpThrowable", idVal, id, true);
                // auto castTmp = new CastExp(exp.loc, tmp, exp.type);

                auto ctor = new DotIdExp(exp.loc, tmp, Id.ctor).expressionSemantic(sc);
                auto ctorCall = new CallExp(exp.loc, ctor, exp.arguments);

                id = Expression.combine(idVal, exp.argprefix).expressionSemantic(sc);
                id = Expression.combine(id, ctorCall).expressionSemantic(sc);
                // id = Expression.combine(id, castTmp).expressionSemantic(sc);

                result = id.expressionSemantic(sc);
                return;
            }
            else if (!exp.onstack && !exp.type.isscope())
            {
                auto hook = global.params.tracegc ? Id._d_newclassTTrace : Id._d_newclassT;
                if (!verifyHookExist(exp.loc, *sc, hook, "new class"))
                    return setError();

                Expression id = new IdentifierExp(exp.loc, Id.empty);
                id = new DotIdExp(exp.loc, id, Id.object);

                auto tiargs = new Objects();
                auto t = exp.newtype.unqualify(MODFlags.wild);  // remove `inout`
                tiargs.push(t);
                id = new DotTemplateInstanceExp(exp.loc, id, hook, tiargs);
                auto arguments = new Expressions();
                if (global.params.tracegc)
                {
                    auto funcname = (sc.callsc && sc.callsc.func) ?
                        sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
                    arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
                    arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
                    arguments.push(new StringExp(exp.loc, funcname.toDString()));
                }
                id = new CallExp(exp.loc, id, arguments);

                exp.lowering = id.expressionSemantic(sc);
            }
        }
        else if (auto ts = tb.isTypeStruct())
        {
            auto sd = ts.sym;
            sd.size(exp.loc);
            if (sd.sizeok != Sizeok.done)
                return setError();
            if (!sd.ctor)
                sd.ctor = sd.searchCtor();
            if (sd.noDefaultCtor && !nargs)
            {
                exp.error("default construction is disabled for type `%s`", sd.type.toChars());
                return setError();
            }
            // checkDeprecated() is already done in newtype.typeSemantic().

            if (sd.disableNew)
            {
                exp.error("cannot allocate `struct %s` with `new` because it is annotated with `@disable new()`",
                          originalNewtype.toChars());
                return setError();
            }

            // https://issues.dlang.org/show_bug.cgi?id=22639
            // If the new expression has arguments, we either should call a
            // regular constructor of a copy constructor if the first argument
            // is the same type as the struct
            if (nargs && (sd.hasRegularCtor() || (sd.ctor && (*exp.arguments)[0].type.mutableOf() == sd.type.mutableOf())))
            {
                FuncDeclaration f = resolveFuncCall(exp.loc, sc, sd.ctor, null, tb, exp.argumentList, FuncResolveFlag.standard);
                if (!f || f.errors)
                    return setError();

                checkFunctionAttributes(exp, sc, f);
                checkAccess(sd, exp.loc, sc, f);

                TypeFunction tf = f.type.isTypeFunction();
                if (!exp.arguments)
                    exp.arguments = new Expressions();
                if (functionParameters(exp.loc, sc, tf, null, exp.type, exp.argumentList, f, &exp.type, &exp.argprefix))
                    return setError();

                exp.member = f.isCtorDeclaration();
                assert(exp.member);

                if (checkFrameAccess(exp.loc, sc, sd, sd.fields.length))
                    return setError();
            }
            else
            {
                if (exp.names)
                {
                    exp.arguments = resolveStructLiteralNamedArgs(sd, exp.type, sc, exp.loc,
                        exp.names ? (*exp.names)[] : null,
                        (size_t i, Type t) => (*exp.arguments)[i],
                        i => (*exp.arguments)[i].loc
                    );
                    if (!exp.arguments)
                        return setError();
                }
                else if (!exp.arguments)
                {
                    exp.arguments = new Expressions();
                }

                if (!sd.fit(exp.loc, sc, exp.arguments, tb))
                    return setError();

                if (!sd.fill(exp.loc, *exp.arguments, false))
                    return setError();

                if (checkFrameAccess(exp.loc, sc, sd, exp.arguments ? exp.arguments.length : 0))
                    return setError();

                /* Since a `new` allocation may escape, check each of the arguments for escaping
                 */
                foreach (arg; *exp.arguments)
                {
                    if (arg && checkNewEscape(sc, arg, false))
                        return setError();
                }
            }

            exp.type = exp.type.pointerTo();
        }
        else if (tb.ty == Tarray)
        {
            if (!nargs)
            {
                // https://issues.dlang.org/show_bug.cgi?id=20422
                // Without this check the compiler would give a misleading error
                exp.error("missing length argument for array");
                return setError();
            }

            Type tn = tb.nextOf().baseElemOf();
            Dsymbol s = tn.toDsymbol(sc);
            AggregateDeclaration ad = s ? s.isAggregateDeclaration() : null;
            if (ad && ad.noDefaultCtor)
            {
                exp.error("default construction is disabled for type `%s`", tb.nextOf().toChars());
                return setError();
            }
            for (size_t i = 0; i < nargs; i++)
            {
                if (tb.ty != Tarray)
                {
                    exp.error("too many arguments for array");
                    return setError();
                }

                Expression arg = (*exp.arguments)[i];
                if (exp.names && (*exp.names)[i])
                {
                    exp.error("no named argument `%s` allowed for array dimension", (*exp.names)[i].toChars());
                    return setError();
                }

                arg = resolveProperties(sc, arg);
                arg = arg.implicitCastTo(sc, Type.tsize_t);
                if (arg.op == EXP.error)
                    return setError();
                arg = arg.optimize(WANTvalue);
                if (arg.op == EXP.int64 && cast(sinteger_t)arg.toInteger() < 0)
                {
                    exp.error("negative array index `%s`", arg.toChars());
                    return setError();
                }
                (*exp.arguments)[i] = arg;
                tb = tb.isTypeDArray().next.toBasetype();
            }
        }
        else if (tb.isscalar())
        {
            if (!nargs)
            {
            }
            else if (nargs == 1)
            {
                if (exp.names && (*exp.names)[0])
                {
                    exp.error("no named argument `%s` allowed for scalar", (*exp.names)[0].toChars());
                    return setError();
                }
                Expression e = (*exp.arguments)[0];
                e = e.implicitCastTo(sc, tb);
                (*exp.arguments)[0] = e;
            }
            else
            {
                exp.error("more than one argument for construction of `%s`", exp.type.toChars());
                return setError();
            }

            exp.type = exp.type.pointerTo();
        }
        else if (tb.ty == Taarray)
        {
            // e.g. `new Alias(args)`
            if (nargs)
            {
                exp.error("`new` cannot take arguments for an associative array");
                return setError();
            }
        }
        else
        {
            exp.error("cannot create a `%s` with `new`", exp.type.toChars());
            return setError();
        }

        //printf("NewExp: '%s'\n", toChars());
        //printf("NewExp:type '%s'\n", type.toChars());
        semanticTypeInfo(sc, exp.type);

        if (newprefix)
        {
            result = Expression.combine(newprefix, exp);
            return;
        }
        result = exp;
    }

    override void visit(NewAnonClassExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("NewAnonClassExp::semantic() %s\n", e.toChars());
            //printf("thisexp = %p\n", thisexp);
            //printf("type: %s\n", type.toChars());
        }

        Expression d = new DeclarationExp(e.loc, e.cd);
        sc = sc.push(); // just create new scope
        sc.flags &= ~SCOPE.ctfe; // temporary stop CTFE
        d = d.expressionSemantic(sc);
        sc = sc.pop();

        if (!e.cd.errors && sc.intypeof && !sc.parent.inNonRoot())
        {
            ScopeDsymbol sds = sc.tinst ? cast(ScopeDsymbol)sc.tinst : sc._module;
            if (!sds.members)
                sds.members = new Dsymbols();
            sds.members.push(e.cd);
        }

        Expression n = new NewExp(e.loc, e.thisexp, e.cd.type, e.arguments);

        Expression c = new CommaExp(e.loc, d, n);
        result = c.expressionSemantic(sc);
    }

    override void visit(SymOffExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("SymOffExp::semantic('%s')\n", e.toChars());
        }
        //var.dsymbolSemantic(sc);
        if (!e.type)
            e.type = e.var.type.pointerTo();

        if (auto v = e.var.isVarDeclaration())
        {
            if (v.checkNestedReference(sc, e.loc))
                return setError();
        }
        else if (auto f = e.var.isFuncDeclaration())
        {
            if (f.checkNestedReference(sc, e.loc))
                return setError();
        }

        result = e;
    }

    override void visit(VarExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("VarExp::semantic(%s)\n", e.toChars());
        }

        auto vd = e.var.isVarDeclaration();
        auto fd = e.var.isFuncDeclaration();

        if (fd)
        {
            //printf("L%d fd = %s\n", __LINE__, f.toChars());
            if (!fd.functionSemantic())
                return setError();
        }

        if (!e.type)
            e.type = e.var.type;
        if (e.type && !e.type.deco)
        {
            auto decl = e.var.isDeclaration();
            if (decl)
                decl.inuse++;
            e.type = e.type.typeSemantic(e.loc, sc);
            if (decl)
                decl.inuse--;
        }

        /* Fix for 1161 doesn't work because it causes visibility
         * problems when instantiating imported templates passing private
         * variables as alias template parameters.
         */
        //checkAccess(loc, sc, NULL, var);

        if (vd)
        {
            if (vd.checkNestedReference(sc, e.loc))
                return setError();

            // https://issues.dlang.org/show_bug.cgi?id=12025
            // If the variable is not actually used in runtime code,
            // the purity violation error is redundant.
            //checkPurity(sc, vd);
        }
        else if (fd)
        {
            // TODO: If fd isn't yet resolved its overload, the checkNestedReference
            // call would cause incorrect validation.
            // Maybe here should be moved in CallExp, or AddrExp for functions.
            if (fd.checkNestedReference(sc, e.loc))
                return setError();
        }
        else if (auto od = e.var.isOverDeclaration())
        {
            e.type = Type.tvoid; // ambiguous type?
        }

        result = e;
    }

    override void visit(FuncExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("FuncExp::semantic(%s)\n", exp.toChars());
            if (exp.fd.treq)
                printf("  treq = %s\n", exp.fd.treq.toChars());
        }

        if (exp.type)
        {
            result = exp;
            return;
        }

        Expression e = exp;
        uint olderrors;

        sc = sc.push(); // just create new scope
        sc.flags &= ~SCOPE.ctfe; // temporary stop CTFE
        sc.visibility = Visibility(Visibility.Kind.public_); // https://issues.dlang.org/show_bug.cgi?id=12506

        /* fd.treq might be incomplete type,
            * so should not semantic it.
            * void foo(T)(T delegate(int) dg){}
            * foo(a=>a); // in IFTI, treq == T delegate(int)
            */
        //if (fd.treq)
        //    fd.treq = fd.treq.dsymbolSemantic(loc, sc);

        exp.genIdent(sc);

        // Set target of return type inference
        if (exp.fd.treq && !exp.fd.type.nextOf())
        {
            TypeFunction tfv = null;
            if (exp.fd.treq.ty == Tdelegate || exp.fd.treq.isPtrToFunction())
                tfv = cast(TypeFunction)exp.fd.treq.nextOf();
            if (tfv)
            {
                TypeFunction tfl = cast(TypeFunction)exp.fd.type;
                tfl.next = tfv.nextOf();
            }
        }

        //printf("td = %p, treq = %p\n", td, fd.treq);
        if (exp.td)
        {
            assert(exp.td.parameters && exp.td.parameters.length);
            exp.td.dsymbolSemantic(sc);
            exp.type = Type.tvoid; // temporary type

            if (exp.fd.treq) // defer type determination
            {
                FuncExp fe;
                if (exp.matchType(exp.fd.treq, sc, &fe) > MATCH.nomatch)
                    e = fe;
                else
                    e = ErrorExp.get();
            }
            goto Ldone;
        }

        olderrors = global.errors;
        exp.fd.dsymbolSemantic(sc);
        if (olderrors == global.errors)
        {
            exp.fd.semantic2(sc);
            if (olderrors == global.errors)
                exp.fd.semantic3(sc);
        }
        if (olderrors != global.errors)
        {
            if (exp.fd.type && exp.fd.type.ty == Tfunction && !exp.fd.type.nextOf())
                (cast(TypeFunction)exp.fd.type).next = Type.terror;
            e = ErrorExp.get();
            goto Ldone;
        }

        // Type is a "delegate to" or "pointer to" the function literal
        if ((exp.fd.isNested() && exp.fd.tok == TOK.delegate_) || (exp.tok == TOK.reserved && exp.fd.treq && exp.fd.treq.ty == Tdelegate))
        {
            // https://issues.dlang.org/show_bug.cgi?id=22686
            // if the delegate return type is an error
            // abort semantic of the FuncExp and propagate
            // the error
            if (exp.fd.type.isTypeError())
            {
                e = ErrorExp.get();
                goto Ldone;
            }
            exp.type = new TypeDelegate(exp.fd.type.isTypeFunction());
            exp.type = exp.type.typeSemantic(exp.loc, sc);

            exp.fd.tok = TOK.delegate_;
        }
        else
        {
            exp.type = new TypePointer(exp.fd.type);
            exp.type = exp.type.typeSemantic(exp.loc, sc);
            //type = fd.type.pointerTo();

            /* A lambda expression deduced to function pointer might become
                * to a delegate literal implicitly.
                *
                *   auto foo(void function() fp) { return 1; }
                *   assert(foo({}) == 1);
                *
                * So, should keep fd.tok == TOK.reserve if fd.treq == NULL.
                */
            if (exp.fd.treq && exp.fd.treq.ty == Tpointer)
            {
                // change to non-nested
                exp.fd.tok = TOK.function_;
                exp.fd.vthis = null;
            }
        }
        exp.fd.tookAddressOf++;

    Ldone:
        sc = sc.pop();
        result = e;
    }

    /**
     * Perform semantic analysis on function literals
     *
     * Test the following construct:
     * ---
     * (x, y, z) { return x + y + z; }(42, 84, 1992);
     * ---
     */
    Expression callExpSemantic(FuncExp exp, Scope* sc, Expressions* arguments)
    {
        if ((!exp.type || exp.type == Type.tvoid) && exp.td && arguments && arguments.length)
        {
            for (size_t k = 0; k < arguments.length; k++)
            {
                Expression checkarg = (*arguments)[k];
                if (checkarg.op == EXP.error)
                    return checkarg;
            }

            exp.genIdent(sc);

            assert(exp.td.parameters && exp.td.parameters.length);
            exp.td.dsymbolSemantic(sc);

            TypeFunction tfl = cast(TypeFunction)exp.fd.type;
            size_t dim = tfl.parameterList.length;
            if (arguments.length < dim)
            {
                // Default arguments are always typed, so they don't need inference.
                Parameter p = tfl.parameterList[arguments.length];
                if (p.defaultArg)
                    dim = arguments.length;
            }

            if ((tfl.parameterList.varargs == VarArg.none && arguments.length > dim) ||
                arguments.length < dim)
            {
                OutBuffer buf;
                foreach (idx, ref arg; *arguments)
                    buf.printf("%s%s", (idx ? ", ".ptr : "".ptr), arg.type.toChars());
                exp.error("function literal `%s%s` is not callable using argument types `(%s)`",
                          exp.fd.toChars(), parametersTypeToChars(tfl.parameterList),
                          buf.peekChars());
                exp.errorSupplemental("too %s arguments, expected %d, got %d",
                                      arguments.length < dim ? "few".ptr : "many".ptr,
                                      cast(int)dim, cast(int)arguments.length);
                return ErrorExp.get();
            }

            auto tiargs = new Objects();
            tiargs.reserve(exp.td.parameters.length);

            for (size_t i = 0; i < exp.td.parameters.length; i++)
            {
                TemplateParameter tp = (*exp.td.parameters)[i];
                assert(dim <= tfl.parameterList.length);
                foreach (u, p; tfl.parameterList)
                {
                    if (u == dim)
                        break;

                    if (p.type.ty == Tident && (cast(TypeIdentifier)p.type).ident == tp.ident)
                    {
                        Expression e = (*arguments)[u];
                        tiargs.push(e.type);
                        break;
                    }
                }
            }

            auto ti = new TemplateInstance(exp.loc, exp.td, tiargs);
            return (new ScopeExp(exp.loc, ti)).expressionSemantic(sc);
        }
        return exp.expressionSemantic(sc);
    }

    override void visit(CallExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("CallExp::semantic() %s\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return; // semantic() already run
        }

        Objects* tiargs = null; // initial list of template arguments
        Expression ethis = null;
        Type tthis = null;
        Expression e1org = exp.e1;

        if (auto ce = exp.e1.isCommaExp())
        {
            /* Rewrite (a,b)(args) as (a,(b(args)))
             */
            exp.e1 = ce.e2;
            ce.e2 = exp;
            result = ce.expressionSemantic(sc);
            return;
        }
        if (DelegateExp de = exp.e1.isDelegateExp())
        {
            exp.e1 = new DotVarExp(de.loc, de.e1, de.func, de.hasOverloads);
            visit(exp);
            return;
        }
        if (FuncExp fe = exp.e1.isFuncExp())
        {
            if (arrayExpressionSemantic(exp.arguments.peekSlice(), sc) ||
                preFunctionParameters(sc, exp.argumentList))
                return setError();

            // Run e1 semantic even if arguments have any errors
            exp.e1 = callExpSemantic(fe, sc, exp.arguments);
            if (exp.e1.op == EXP.error)
            {
                result = exp.e1;
                return;
            }
        }
        if (sc.flags & SCOPE.Cfile)
        {
            /* See if need to rewrite the AST because of cast/call ambiguity
             */
            if (auto e = castCallAmbiguity(exp, sc))
            {
                result = expressionSemantic(e, sc);
                return;
            }
        }

        if (Expression ex = resolveUFCS(sc, exp))
        {
            result = ex;
            return;
        }

        /* This recognizes:
         *  foo!(tiargs)(funcargs)
         */
        if (ScopeExp se = exp.e1.isScopeExp())
        {
            TemplateInstance ti = se.sds.isTemplateInstance();
            if (ti)
            {
                /* Attempt to instantiate ti. If that works, go with it.
                 * If not, go with partial explicit specialization.
                 */
                WithScopeSymbol withsym;
                if (!ti.findTempDecl(sc, &withsym) || !ti.semanticTiargs(sc))
                    return setError();
                if (withsym && withsym.withstate.wthis)
                {
                    exp.e1 = new VarExp(exp.e1.loc, withsym.withstate.wthis);
                    exp.e1 = new DotTemplateInstanceExp(exp.e1.loc, exp.e1, ti);
                    goto Ldotti;
                }
                if (ti.needsTypeInference(sc, 1))
                {
                    /* Go with partial explicit specialization
                     */
                    tiargs = ti.tiargs;
                    assert(ti.tempdecl);
                    if (TemplateDeclaration td = ti.tempdecl.isTemplateDeclaration())
                        exp.e1 = new TemplateExp(exp.loc, td);
                    else if (OverDeclaration od = ti.tempdecl.isOverDeclaration())
                        exp.e1 = new VarExp(exp.loc, od);
                    else
                        exp.e1 = new OverExp(exp.loc, ti.tempdecl.isOverloadSet());
                }
                else
                {
                    Expression e1x = exp.e1.expressionSemantic(sc);
                    if (e1x.op == EXP.error)
                    {
                        result = e1x;
                        return;
                    }
                    exp.e1 = e1x;
                }
            }
        }

        /* This recognizes:
         *  expr.foo!(tiargs)(funcargs)
         */
    Ldotti:
        if (DotTemplateInstanceExp se = exp.e1.isDotTemplateInstanceExp())
        {
            TemplateInstance ti = se.ti;
            {
                /* Attempt to instantiate ti. If that works, go with it.
                 * If not, go with partial explicit specialization.
                 */
                if (!se.findTempDecl(sc) || !ti.semanticTiargs(sc))
                    return setError();
                if (ti.needsTypeInference(sc, 1))
                {
                    /* Go with partial explicit specialization
                     */
                    tiargs = ti.tiargs;
                    assert(ti.tempdecl);
                    if (TemplateDeclaration td = ti.tempdecl.isTemplateDeclaration())
                        exp.e1 = new DotTemplateExp(exp.loc, se.e1, td);
                    else if (OverDeclaration od = ti.tempdecl.isOverDeclaration())
                    {
                        exp.e1 = new DotVarExp(exp.loc, se.e1, od, true);
                    }
                    else
                        exp.e1 = new DotExp(exp.loc, se.e1, new OverExp(exp.loc, ti.tempdecl.isOverloadSet()));
                }
                else
                {
                    Expression e1x = exp.e1.expressionSemantic(sc);
                    if (e1x.op == EXP.error)
                    {
                        result = e1x;
                        return;
                    }
                    exp.e1 = e1x;
                }
            }
        }

    Lagain:
        //printf("Lagain: %s\n", toChars());
        exp.f = null;
        if (exp.e1.op == EXP.this_ || exp.e1.op == EXP.super_)
        {
            // semantic() run later for these
        }
        else
        {
            if (DotIdExp die = exp.e1.isDotIdExp())
            {
                exp.e1 = die.expressionSemantic(sc);
                /* Look for e1 having been rewritten to expr.opDispatch!(string)
                 * We handle such earlier, so go back.
                 * Note that in the rewrite, we carefully did not run semantic() on e1
                 */
                if (exp.e1.op == EXP.dotTemplateInstance)
                {
                    goto Ldotti;
                }
            }
            else
            {
                __gshared int nest;
                if (++nest > global.recursionLimit)
                {
                    exp.error("recursive evaluation of `%s`", exp.toChars());
                    --nest;
                    return setError();
                }
                Expression ex = unaSemantic(exp, sc);
                --nest;
                if (ex)
                {
                    result = ex;
                    return;
                }
            }

            /* Look for e1 being a lazy parameter
             */
            if (VarExp ve = exp.e1.isVarExp())
            {
                if (ve.var.storage_class & STC.lazy_)
                {
                    // lazy parameters can be called without violating purity and safety
                    Type tw = ve.var.type;
                    Type tc = ve.var.type.substWildTo(MODFlags.const_);
                    auto tf = new TypeFunction(ParameterList(), tc, LINK.d, STC.safe | STC.pure_);
                    (tf = cast(TypeFunction)tf.typeSemantic(exp.loc, sc)).next = tw; // hack for bug7757
                    auto t = new TypeDelegate(tf);
                    ve.type = t.typeSemantic(exp.loc, sc);
                }
                VarDeclaration v = ve.var.isVarDeclaration();
                if (v && ve.checkPurity(sc, v))
                    return setError();
            }

            if (exp.e1.op == EXP.symbolOffset && (cast(SymOffExp)exp.e1).hasOverloads)
            {
                SymOffExp se = cast(SymOffExp)exp.e1;
                exp.e1 = new VarExp(se.loc, se.var, true);
                exp.e1 = exp.e1.expressionSemantic(sc);
            }
            else if (DotExp de = exp.e1.isDotExp())
            {
                if (de.e2.op == EXP.overloadSet)
                {
                    ethis = de.e1;
                    tthis = de.e1.type;
                    exp.e1 = de.e2;
                }
            }
            else if (exp.e1.op == EXP.star && exp.e1.type.ty == Tfunction)
            {
                // Rewrite (*fp)(arguments) to fp(arguments)
                exp.e1 = (cast(PtrExp)exp.e1).e1;
            }
            else if (exp.e1.op == EXP.type && (sc && sc.flags & SCOPE.Cfile))
            {
                const numArgs = exp.arguments ? exp.arguments.length : 0;

                /* Ambiguous cases arise from CParser where there is not enough
                 * information to determine if we have a function call or declaration.
                 *   type-name ( identifier ) ;
                 *   identifier ( identifier ) ;
                 * If exp.e1 is a type-name, then this is a declaration. C11 does not
                 * have type construction syntax, so don't convert this to a cast().
                 */
                if (numArgs == 1)
                {
                    Expression arg = (*exp.arguments)[0];
                    if (auto ie = (*exp.arguments)[0].isIdentifierExp())
                    {
                        TypeExp te = cast(TypeExp)exp.e1;
                        auto initializer = new VoidInitializer(ie.loc);
                        Dsymbol s = new VarDeclaration(ie.loc, te.type, ie.ident, initializer);
                        auto decls = new Dsymbols(1);
                        (*decls)[0] = s;
                        s = new LinkDeclaration(s.loc, LINK.c, decls);
                        result = new DeclarationExp(exp.loc, s);
                        result = result.expressionSemantic(sc);
                    }
                    else
                    {
                        arg.error("identifier or `(` expected");
                        result = ErrorExp.get();
                    }
                    return;
                }
                exp.error("identifier or `(` expected before `)`");
                result = ErrorExp.get();
                return;
            }
        }

        Type t1 = exp.e1.type ? exp.e1.type.toBasetype() : null;

        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        if (arrayExpressionSemantic(exp.arguments.peekSlice(), sc) ||
            preFunctionParameters(sc, exp.argumentList))
            return setError();

        // Check for call operator overload
        if (t1)
        {
            if (t1.ty == Tstruct)
            {
                auto sd = (cast(TypeStruct)t1).sym;
                sd.size(exp.loc); // Resolve forward references to construct object
                if (sd.sizeok != Sizeok.done)
                    return setError();
                if (!sd.ctor)
                    sd.ctor = sd.searchCtor();
                /* If `sd.ctor` is a generated copy constructor, this means that it
                   is the single constructor that this struct has. In order to not
                   disable default construction, the ctor is nullified. The side effect
                   of this is that the generated copy constructor cannot be called
                   explicitly, but that is ok, because when calling a constructor the
                   default constructor should have priority over the generated copy
                   constructor.
                */
                if (sd.ctor)
                {
                    auto ctor = sd.ctor.isCtorDeclaration();
                    if (ctor && ctor.isCpCtor && ctor.isGenerated())
                        sd.ctor = null;
                }

                // First look for constructor
                if (exp.e1.op == EXP.type && sd.ctor)
                {
                    if (!sd.noDefaultCtor && !(exp.arguments && exp.arguments.length))
                        goto Lx;

                    /* https://issues.dlang.org/show_bug.cgi?id=20695
                       If all constructors are copy constructors, then
                       try default construction.
                     */
                    if (!sd.hasRegularCtor &&
                        // https://issues.dlang.org/show_bug.cgi?id=22639
                        // we might still have a copy constructor that could be called
                        (*exp.arguments)[0].type.mutableOf != sd.type.mutableOf())
                        goto Lx;

                    auto sle = new StructLiteralExp(exp.loc, sd, null, exp.e1.type);
                    if (!sd.fill(exp.loc, *sle.elements, true))
                        return setError();
                    if (checkFrameAccess(exp.loc, sc, sd, sle.elements.length))
                        return setError();

                    // https://issues.dlang.org/show_bug.cgi?id=14556
                    // Set concrete type to avoid further redundant semantic().
                    sle.type = exp.e1.type;

                    /* Constructor takes a mutable object, so don't use
                     * the immutable initializer symbol.
                     */
                    sle.useStaticInit = false;

                    Expression e = sle;
                    if (auto cf = sd.ctor.isCtorDeclaration())
                    {
                        e = new DotVarExp(exp.loc, e, cf, true);
                    }
                    else if (auto td = sd.ctor.isTemplateDeclaration())
                    {
                        e = new DotIdExp(exp.loc, e, td.ident);
                    }
                    else if (auto os = sd.ctor.isOverloadSet())
                    {
                        e = new DotExp(exp.loc, e, new OverExp(exp.loc, os));
                    }
                    else
                        assert(0);
                    e = new CallExp(exp.loc, e, exp.arguments);
                    e = e.expressionSemantic(sc);
                    result = e;
                    return;
                }
                // No constructor, look for overload of opCall
                if (search_function(sd, Id.call))
                    goto L1;
                // overload of opCall, therefore it's a call
                if (exp.e1.op != EXP.type)
                {
                    if (sd.aliasthis && !isRecursiveAliasThis(exp.att1, exp.e1.type))
                    {
                        exp.e1 = resolveAliasThis(sc, exp.e1);
                        goto Lagain;
                    }
                    exp.error("%s `%s` does not overload ()", sd.kind(), sd.toChars());
                    return setError();
                }

                /* It's a struct literal
                 */
            Lx:
                Expressions* resolvedArgs = exp.arguments;
                if (exp.names)
                {
                    resolvedArgs = resolveStructLiteralNamedArgs(sd, exp.e1.type, sc, exp.loc,
                        (*exp.names)[],
                        (size_t i, Type t) => (*exp.arguments)[i],
                        i => (*exp.arguments)[i].loc
                    );
                    if (!resolvedArgs)
                    {
                        result = ErrorExp.get();
                        return;
                    }
                }

                Expression e = new StructLiteralExp(exp.loc, sd, resolvedArgs, exp.e1.type);
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }
            else if (t1.ty == Tclass)
            {
            L1:
                // Rewrite as e1.call(arguments)
                Expression e = new DotIdExp(exp.loc, exp.e1, Id.call);
                e = new CallExp(exp.loc, e, exp.arguments, exp.names);
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }
            else if (exp.e1.op == EXP.type && t1.isscalar())
            {
                Expression e;

                // Make sure to use the enum type itself rather than its
                // base type
                // https://issues.dlang.org/show_bug.cgi?id=16346
                if (exp.e1.type.ty == Tenum)
                {
                    t1 = exp.e1.type;
                }

                if (!exp.arguments || exp.arguments.length == 0)
                {
                    e = t1.defaultInitLiteral(exp.loc);
                }
                else if (exp.arguments.length == 1)
                {
                    e = (*exp.arguments)[0];
                    e = e.implicitCastTo(sc, t1);
                    e = new CastExp(exp.loc, e, t1);
                }
                else
                {
                    exp.error("more than one argument for construction of `%s`", t1.toChars());
                    return setError();
                }
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }
        }

        FuncDeclaration resolveOverloadSet(Loc loc, Scope* sc,
            OverloadSet os, Objects* tiargs, Type tthis, ArgumentList argumentList)
        {
            FuncDeclaration f = null;
            foreach (s; os.a)
            {
                if (tiargs && s.isFuncDeclaration())
                    continue;
                if (auto f2 = resolveFuncCall(loc, sc, s, tiargs, tthis, argumentList, FuncResolveFlag.quiet))
                {
                    if (f2.errors)
                        return null;
                    if (f)
                    {
                        /* Error if match in more than one overload set,
                         * even if one is a 'better' match than the other.
                         */
                        ScopeDsymbol.multiplyDefined(loc, f, f2);
                    }
                    else
                        f = f2;
                }
            }
            if (!f)
            {
                .error(loc, "no overload matches for `%s`", exp.toChars());
                errorSupplemental(loc, "Candidates are:");
                foreach (s; os.a)
                {
                    overloadApply(s, (ds){
                        if (auto fd = ds.isFuncDeclaration())
                            .errorSupplemental(ds.loc, "%s%s", fd.toChars(),
                                fd.type.toTypeFunction().parameterList.parametersTypeToChars());
                        else
                            .errorSupplemental(ds.loc, "%s", ds.toChars());
                        return 0;
                    });
                }
            }
            else if (f.errors)
                f = null;
            return f;
        }

        bool isSuper = false;
        if (exp.e1.op == EXP.dotVariable && t1.ty == Tfunction || exp.e1.op == EXP.dotTemplateDeclaration)
        {
            UnaExp ue = cast(UnaExp)exp.e1;

            Expression ue1old = ue.e1; // need for 'right this' check
            DotVarExp dve;
            DotTemplateExp dte;
            Dsymbol s;
            if (exp.e1.op == EXP.dotVariable)
            {
                dve = cast(DotVarExp)exp.e1;
                dte = null;
                s = dve.var;
                tiargs = null;
            }
            else
            {
                dve = null;
                dte = cast(DotTemplateExp)exp.e1;
                s = dte.td;
            }

            // Do overload resolution
            exp.f = resolveFuncCall(exp.loc, sc, s, tiargs, ue.e1.type, exp.argumentList, FuncResolveFlag.standard);
            if (!exp.f || exp.f.errors || exp.f.type.ty == Terror)
                return setError();

            if (exp.f.interfaceVirtual)
            {
                /* Cast 'this' to the type of the interface, and replace f with the interface's equivalent
                 */
                auto b = exp.f.interfaceVirtual;
                auto ad2 = b.sym;
                ue.e1 = ue.e1.castTo(sc, ad2.type.addMod(ue.e1.type.mod));
                ue.e1 = ue.e1.expressionSemantic(sc);
                auto vi = exp.f.findVtblIndex(&ad2.vtbl, cast(int)ad2.vtbl.length);
                assert(vi >= 0);
                exp.f = ad2.vtbl[vi].isFuncDeclaration();
                assert(exp.f);
            }
            if (exp.f.needThis())
            {
                AggregateDeclaration ad = exp.f.isMemberLocal();
                ue.e1 = getRightThis(exp.loc, sc, ad, ue.e1, exp.f);
                if (ue.e1.op == EXP.error)
                {
                    result = ue.e1;
                    return;
                }
                ethis = ue.e1;
                tthis = ue.e1.type;
                if (!(exp.f.type.ty == Tfunction && (cast(TypeFunction)exp.f.type).isScopeQual))
                {
                    if (checkParamArgumentEscape(sc, exp.f, Id.This, exp.f.vthis, STC.undefined_, ethis, false, false))
                        return setError();
                }
            }

            /* Cannot call public functions from inside invariant
             * (because then the invariant would have infinite recursion)
             */
            if (sc.func && sc.func.isInvariantDeclaration() && ue.e1.op == EXP.this_ && exp.f.addPostInvariant())
            {
                exp.error("cannot call `public`/`export` function `%s` from invariant", exp.f.toChars());
                return setError();
            }

            if (!exp.ignoreAttributes)
                checkFunctionAttributes(exp, sc, exp.f);
            checkAccess(exp.loc, sc, ue.e1, exp.f);
            if (!exp.f.needThis())
            {
                exp.e1 = Expression.combine(ue.e1, new VarExp(exp.loc, exp.f, false));
            }
            else
            {
                if (ue1old.checkRightThis(sc))
                    return setError();
                if (exp.e1.op == EXP.dotVariable)
                {
                    dve.var = exp.f;
                    exp.e1.type = exp.f.type;
                }
                else
                {
                    exp.e1 = new DotVarExp(exp.loc, dte.e1, exp.f, false);
                    exp.e1 = exp.e1.expressionSemantic(sc);
                    if (exp.e1.op == EXP.error)
                        return setError();
                    ue = cast(UnaExp)exp.e1;
                }
                version (none)
                {
                    printf("ue.e1 = %s\n", ue.e1.toChars());
                    printf("f = %s\n", exp.f.toChars());
                    printf("t1 = %s\n", t1.toChars());
                    printf("e1 = %s\n", exp.e1.toChars());
                    printf("e1.type = %s\n", exp.e1.type.toChars());
                }

                // See if we need to adjust the 'this' pointer
                AggregateDeclaration ad = exp.f.isThis();
                ClassDeclaration cd = ue.e1.type.isClassHandle();
                if (ad && cd && ad.isClassDeclaration())
                {
                    if (ue.e1.op == EXP.dotType)
                    {
                        ue.e1 = (cast(DotTypeExp)ue.e1).e1;
                        exp.directcall = true;
                    }
                    else if (ue.e1.op == EXP.super_)
                        exp.directcall = true;
                    else if ((cd.storage_class & STC.final_) != 0) // https://issues.dlang.org/show_bug.cgi?id=14211
                        exp.directcall = true;

                    if (ad != cd)
                    {
                        ue.e1 = ue.e1.castTo(sc, ad.type.addMod(ue.e1.type.mod));
                        ue.e1 = ue.e1.expressionSemantic(sc);
                    }
                }
            }
            // If we've got a pointer to a function then deference it
            // https://issues.dlang.org/show_bug.cgi?id=16483
            if (exp.e1.type.isPtrToFunction())
            {
                Expression e = new PtrExp(exp.loc, exp.e1);
                e.type = exp.e1.type.nextOf();
                exp.e1 = e;
            }
            t1 = exp.e1.type;
        }
        else if (exp.e1.op == EXP.super_ || exp.e1.op == EXP.this_)
        {
            auto ad = sc.func ? sc.func.isThis() : null;
            auto cd = ad ? ad.isClassDeclaration() : null;

            isSuper = exp.e1.op == EXP.super_;
            if (isSuper)
            {
                // Base class constructor call
                if (!cd || !cd.baseClass || !sc.func.isCtorDeclaration())
                {
                    exp.error("super class constructor call must be in a constructor");
                    return setError();
                }
                if (!cd.baseClass.ctor)
                {
                    exp.error("no super class constructor for `%s`", cd.baseClass.toChars());
                    return setError();
                }
            }
            else
            {
                // `this` call expression must be inside a
                // constructor
                if (!ad || !sc.func.isCtorDeclaration())
                {
                    exp.error("constructor call must be in a constructor");
                    return setError();
                }

                // https://issues.dlang.org/show_bug.cgi?id=18719
                // If `exp` is a call expression to another constructor
                // then it means that all struct/class fields will be
                // initialized after this call.
                foreach (ref field; sc.ctorflow.fieldinit)
                {
                    field.csx |= CSX.this_ctor;
                }
            }

            if (!sc.intypeof && !(sc.ctorflow.callSuper & CSX.halt))
            {
                if (sc.inLoop || sc.ctorflow.callSuper & CSX.label)
                    exp.error("constructor calls not allowed in loops or after labels");
                if (sc.ctorflow.callSuper & (CSX.super_ctor | CSX.this_ctor))
                    exp.error("multiple constructor calls");
                if ((sc.ctorflow.callSuper & CSX.return_) && !(sc.ctorflow.callSuper & CSX.any_ctor))
                    exp.error("an earlier `return` statement skips constructor");
                sc.ctorflow.callSuper |= CSX.any_ctor | (isSuper ? CSX.super_ctor : CSX.this_ctor);
            }

            tthis = ad.type.addMod(sc.func.type.mod);
            auto ctor = isSuper ? cd.baseClass.ctor : ad.ctor;
            if (auto os = ctor.isOverloadSet())
                exp.f = resolveOverloadSet(exp.loc, sc, os, null, tthis, exp.argumentList);
            else
                exp.f = resolveFuncCall(exp.loc, sc, ctor, null, tthis, exp.argumentList, FuncResolveFlag.standard);

            if (!exp.f || exp.f.errors)
                return setError();

            checkFunctionAttributes(exp, sc, exp.f);
            checkAccess(exp.loc, sc, null, exp.f);

            exp.e1 = new DotVarExp(exp.e1.loc, exp.e1, exp.f, false);
            exp.e1 = exp.e1.expressionSemantic(sc);
            // https://issues.dlang.org/show_bug.cgi?id=21095
            if (exp.e1.op == EXP.error)
                return setError();
            t1 = exp.e1.type;

            // BUG: this should really be done by checking the static
            // call graph
            if (exp.f == sc.func)
            {
                exp.error("cyclic constructor call");
                return setError();
            }
        }
        else if (auto oe = exp.e1.isOverExp())
        {
            exp.f = resolveOverloadSet(exp.loc, sc, oe.vars, tiargs, tthis, exp.argumentList);
            if (!exp.f)
                return setError();
            if (ethis)
                exp.e1 = new DotVarExp(exp.loc, ethis, exp.f, false);
            else
                exp.e1 = new VarExp(exp.loc, exp.f, false);
            goto Lagain;
        }
        else if (!t1)
        {
            exp.error("function expected before `()`, not `%s`", exp.e1.toChars());
            return setError();
        }
        else if (t1.ty == Terror)
        {
            return setError();
        }
        else if (t1.ty != Tfunction)
        {
            TypeFunction tf;
            const(char)* p;
            Dsymbol s;
            exp.f = null;
            if (auto fe = exp.e1.isFuncExp())
            {
                // function literal that direct called is always inferred.
                assert(fe.fd);
                exp.f = fe.fd;
                tf = cast(TypeFunction)exp.f.type;
                p = "function literal";
            }
            else if (t1.ty == Tdelegate)
            {
                TypeDelegate td = cast(TypeDelegate)t1;
                assert(td.next.ty == Tfunction);
                tf = cast(TypeFunction)td.next;
                p = "delegate";
            }
            else if (auto tfx = t1.isPtrToFunction())
            {
                tf = tfx;
                p = "function pointer";
            }
            else if (exp.e1.op == EXP.dotVariable && (cast(DotVarExp)exp.e1).var.isOverDeclaration())
            {
                DotVarExp dve = cast(DotVarExp)exp.e1;
                exp.f = resolveFuncCall(exp.loc, sc, dve.var, tiargs, dve.e1.type, exp.argumentList, FuncResolveFlag.overloadOnly);
                if (!exp.f)
                    return setError();
                if (exp.f.needThis())
                {
                    dve.var = exp.f;
                    dve.type = exp.f.type;
                    dve.hasOverloads = false;
                    goto Lagain;
                }
                exp.e1 = new VarExp(dve.loc, exp.f, false);
                Expression e = new CommaExp(exp.loc, dve.e1, exp);
                result = e.expressionSemantic(sc);
                return;
            }
            else if (exp.e1.op == EXP.variable && (cast(VarExp)exp.e1).var.isOverDeclaration())
            {
                s = (cast(VarExp)exp.e1).var;
                goto L2;
            }
            else if (exp.e1.op == EXP.template_)
            {
                s = (cast(TemplateExp)exp.e1).td;
            L2:
                exp.f = resolveFuncCall(exp.loc, sc, s, tiargs, null, exp.argumentList, FuncResolveFlag.standard);
                if (!exp.f || exp.f.errors)
                    return setError();
                if (exp.f.needThis())
                {
                    if (hasThis(sc))
                    {
                        // Supply an implicit 'this', as in
                        //    this.ident
                        exp.e1 = new DotVarExp(exp.loc, (new ThisExp(exp.loc)).expressionSemantic(sc), exp.f, false);
                        goto Lagain;
                    }
                    else if (isNeedThisScope(sc, exp.f))
                    {
                        exp.error("need `this` for `%s` of type `%s`", exp.f.toChars(), exp.f.type.toChars());
                        return setError();
                    }
                }
                exp.e1 = new VarExp(exp.e1.loc, exp.f, false);
                goto Lagain;
            }
            else
            {
                exp.error("function expected before `()`, not `%s` of type `%s`", exp.e1.toChars(), exp.e1.type.toChars());
                return setError();
            }

            const(char)* failMessage;
            if (!tf.callMatch(null, exp.argumentList, 0, &failMessage, sc))
            {
                OutBuffer buf;
                buf.writeByte('(');
                argExpTypesToCBuffer(&buf, exp.arguments);
                buf.writeByte(')');
                if (tthis)
                    tthis.modToBuffer(&buf);

                //printf("tf = %s, args = %s\n", tf.deco, (*arguments)[0].type.deco);
                .error(exp.loc, "%s `%s%s` is not callable using argument types `%s`",
                    p, exp.e1.toChars(), parametersTypeToChars(tf.parameterList), buf.peekChars());
                if (failMessage)
                    errorSupplemental(exp.loc, "%s", failMessage);
                return setError();
            }
            // Purity and safety check should run after testing arguments matching
            if (exp.f)
            {
                exp.checkPurity(sc, exp.f);
                exp.checkSafety(sc, exp.f);
                exp.checkNogc(sc, exp.f);
                if (exp.f.checkNestedReference(sc, exp.loc))
                    return setError();
            }
            else if (sc.func && sc.intypeof != 1 && !(sc.flags & (SCOPE.ctfe | SCOPE.debug_)))
            {
                bool err = false;
                if (!tf.purity && sc.func.setImpure())
                {
                    exp.error("`pure` %s `%s` cannot call impure %s `%s`",
                        sc.func.kind(), sc.func.toPrettyChars(), p, exp.e1.toChars());
                    err = true;
                }
                if (!tf.isnogc && sc.func.setGC())
                {
                    exp.error("`@nogc` %s `%s` cannot call non-@nogc %s `%s`",
                        sc.func.kind(), sc.func.toPrettyChars(), p, exp.e1.toChars());
                    err = true;
                }
                if (tf.trust <= TRUST.system && sc.setUnsafe(true, exp.loc,
                    "`@safe` function `%s` cannot call `@system` `%s`", sc.func, exp.e1))
                {
                    exp.error("`@safe` %s `%s` cannot call `@system` %s `%s`",
                        sc.func.kind(), sc.func.toPrettyChars(), p, exp.e1.toChars());
                    err = true;
                }
                if (err)
                    return setError();
            }

            if (t1.ty == Tpointer)
            {
                Expression e = new PtrExp(exp.loc, exp.e1);
                e.type = tf;
                exp.e1 = e;
            }
            t1 = tf;
        }
        else if (VarExp ve = exp.e1.isVarExp())
        {
            // Do overload resolution
            exp.f = ve.var.isFuncDeclaration();
            assert(exp.f);
            tiargs = null;

            if (exp.f.overnext)
                exp.f = resolveFuncCall(exp.loc, sc, exp.f, tiargs, null, exp.argumentList, FuncResolveFlag.overloadOnly);
            else
            {
                exp.f = exp.f.toAliasFunc();
                TypeFunction tf = cast(TypeFunction)exp.f.type;
                const(char)* failMessage;
                if (!tf.callMatch(null, exp.argumentList, 0, &failMessage, sc))
                {
                    OutBuffer buf;
                    buf.writeByte('(');
                    argExpTypesToCBuffer(&buf, exp.arguments);
                    buf.writeByte(')');

                    //printf("tf = %s, args = %s\n", tf.deco, (*arguments)[0].type.deco);
                    .error(exp.loc, "%s `%s%s` is not callable using argument types `%s`",
                        exp.f.kind(), exp.f.toPrettyChars(), parametersTypeToChars(tf.parameterList), buf.peekChars());
                    if (failMessage)
                        errorSupplemental(exp.loc, "%s", failMessage);
                    exp.f = null;
                }
            }
            if (!exp.f || exp.f.errors)
                return setError();

            if (exp.f.needThis())
            {
                // Change the ancestor lambdas to delegate before hasThis(sc) call.
                if (exp.f.checkNestedReference(sc, exp.loc))
                    return setError();

                auto memberFunc = hasThis(sc);
                if (memberFunc && haveSameThis(memberFunc, exp.f))
                {
                    // Supply an implicit 'this', as in
                    //    this.ident
                    exp.e1 = new DotVarExp(exp.loc, (new ThisExp(exp.loc)).expressionSemantic(sc), ve.var);
                    // Note: we cannot use f directly, because further overload resolution
                    // through the supplied 'this' may cause different result.
                    goto Lagain;
                }
                else if (isNeedThisScope(sc, exp.f))
                {
                    // At this point it is possible that `exp.f` had an ambiguity error that was
                    // silenced because the previous call to `resolveFuncCall` was done using
                    // `FuncResolveFlag.overloadOnly`. To make sure that a proper error message
                    // is printed, redo the call with `FuncResolveFlag.standard`.
                    //
                    // https://issues.dlang.org/show_bug.cgi?id=22157
                    if (exp.f.overnext)
                        exp.f = resolveFuncCall(exp.loc, sc, exp.f, tiargs, null, exp.argumentList, FuncResolveFlag.standard);

                    if (!exp.f || exp.f.errors)
                        return setError();

                    // If no error is printed, it means that `f` is the single matching overload
                    // and it needs `this`.
                    exp.error("need `this` for `%s` of type `%s`", exp.f.toChars(), exp.f.type.toChars());
                    return setError();
                }
            }

            checkFunctionAttributes(exp, sc, exp.f);
            checkAccess(exp.loc, sc, null, exp.f);
            if (exp.f.checkNestedReference(sc, exp.loc))
                return setError();

            ethis = null;
            tthis = null;

            if (ve.hasOverloads)
            {
                exp.e1 = new VarExp(ve.loc, exp.f, false);
                exp.e1.type = exp.f.type;
            }
            t1 = exp.f.type;
        }
        assert(t1.ty == Tfunction);

        Expression argprefix;
        if (!exp.arguments)
            exp.arguments = new Expressions();
        if (functionParameters(exp.loc, sc, cast(TypeFunction)t1, ethis, tthis, exp.argumentList, exp.f, &exp.type, &argprefix))
            return setError();

        if (!exp.type)
        {
            exp.e1 = e1org; // https://issues.dlang.org/show_bug.cgi?id=10922
                        // avoid recursive expression printing
            exp.error("forward reference to inferred return type of function call `%s`", exp.toChars());
            return setError();
        }

        if (exp.f && exp.f.tintro)
        {
            Type t = exp.type;
            int offset = 0;
            TypeFunction tf = cast(TypeFunction)exp.f.tintro;
            if (tf.next.isBaseOf(t, &offset) && offset)
            {
                exp.type = tf.next;
                result = Expression.combine(argprefix, exp.castTo(sc, t));
                return;
            }
        }

        // Handle the case of a direct lambda call
        if (exp.f && exp.f.isFuncLiteralDeclaration() && sc.func && !sc.intypeof)
        {
            exp.f.tookAddressOf = 0;
        }

        result = Expression.combine(argprefix, exp);

        if (isSuper)
        {
            auto ad = sc.func ? sc.func.isThis() : null;
            auto cd = ad ? ad.isClassDeclaration() : null;
            if (cd && cd.classKind == ClassKind.cpp && exp.f && !exp.f.fbody)
            {
                // if super is defined in C++, it sets the vtable pointer to the base class
                // so we have to restore it, but still return 'this' from super() call:
                // (auto __vptrTmp = this.__vptr, auto __superTmp = super()), (this.__vptr = __vptrTmp, __superTmp)
                Loc loc = exp.loc;

                auto vptr = new DotIdExp(loc, new ThisExp(loc), Id.__vptr);
                auto vptrTmpDecl = copyToTemp(0, "__vptrTmp", vptr);
                auto declareVptrTmp = new DeclarationExp(loc, vptrTmpDecl);

                auto superTmpDecl = copyToTemp(0, "__superTmp", result);
                auto declareSuperTmp = new DeclarationExp(loc, superTmpDecl);

                auto declareTmps = new CommaExp(loc, declareVptrTmp, declareSuperTmp);

                auto restoreVptr = new AssignExp(loc, vptr.syntaxCopy(), new VarExp(loc, vptrTmpDecl));

                Expression e = new CommaExp(loc, declareTmps, new CommaExp(loc, restoreVptr, new VarExp(loc, superTmpDecl)));
                result = e.expressionSemantic(sc);
            }
        }

        // declare dual-context container
        if (exp.f && exp.f.hasDualContext() && !sc.intypeof && sc.func)
        {
            // check access to second `this`
            if (AggregateDeclaration ad2 = exp.f.isMember2())
            {
                Expression te = new ThisExp(exp.loc).expressionSemantic(sc);
                if (te.op != EXP.error)
                    te = getRightThis(exp.loc, sc, ad2, te, exp.f);
                if (te.op == EXP.error)
                {
                    exp.error("need `this` of type `%s` to call function `%s`", ad2.toChars(), exp.f.toChars());
                    return setError();
                }
            }
            exp.vthis2 = makeThis2Argument(exp.loc, sc, exp.f);
            Expression de = new DeclarationExp(exp.loc, exp.vthis2);
            result = Expression.combine(de, result);
            result = result.expressionSemantic(sc);
        }
    }

    override void visit(DeclarationExp e)
    {
        if (e.type)
        {
            result = e;
            return;
        }
        static if (LOGSEMANTIC)
        {
            printf("DeclarationExp::semantic() %s\n", e.toChars());
        }

        uint olderrors = global.errors;

        /* This is here to support extern(linkage) declaration,
         * where the extern(linkage) winds up being an AttribDeclaration
         * wrapper.
         */
        Dsymbol s = e.declaration;

        while (1)
        {
            AttribDeclaration ad = s.isAttribDeclaration();
            if (ad)
            {
                if (ad.decl && ad.decl.length == 1)
                {
                    s = (*ad.decl)[0];
                    continue;
                }
            }
            break;
        }

        //printf("inserting '%s' %p into sc = %p\n", s.toChars(), s, sc);
        // Insert into both local scope and function scope.
        // Must be unique in both.
        if (s.ident)
        {
            VarDeclaration v = s.isVarDeclaration();
            if (v)
            {
                if (sc.flags & SCOPE.Cfile)
                {
                    /* Do semantic() on the type before inserting v into the symbol table
                     */
                    if (!v.originalType)
                        v.originalType = v.type.syntaxCopy();
                    Scope* sc2 = sc.push();
                    sc2.stc |= v.storage_class & STC.FUNCATTR;
                    sc2.linkage = LINK.c;       // account for the extern(C) in front of the declaration
                    v.inuse++;
                    v.type = v.type.typeSemantic(v.loc, sc2);
                    v.inuse--;
                    sc2.pop();
                }
                else
                {
                    /* Do semantic() on initializer first so this will be illegal:
                     *      int a = a;
                     */
                    e.declaration.dsymbolSemantic(sc);
                    s.parent = sc.parent;
                }
            }

            if (!sc.insert(s))
            {
                auto conflict = sc.search(Loc.initial, s.ident, null);
                e.error("declaration `%s` is already defined", s.toPrettyChars());
                errorSupplemental(conflict.loc, "`%s` `%s` is defined here",
                                  conflict.kind(), conflict.toChars());
                return setError();
            }

            if (v && (sc.flags & SCOPE.Cfile))
            {
                /* Do semantic() on initializer last so this will be legal:
                 *      int a = a;
                 */
                e.declaration.dsymbolSemantic(sc);
                s.parent = sc.parent;
            }

            if (sc.func)
            {
                // https://issues.dlang.org/show_bug.cgi?id=11720
                if ((s.isFuncDeclaration() ||
                     s.isAggregateDeclaration() ||
                     s.isEnumDeclaration() ||
                     s.isTemplateDeclaration() ||
                     v
                    ) && !sc.func.localsymtab.insert(s))
                {
                    // Get the previous symbol
                    Dsymbol originalSymbol = sc.func.localsymtab.lookup(s.ident);

                    // Perturb the name mangling so that the symbols can co-exist
                    // instead of colliding
                    s.localNum = cast(ushort)(originalSymbol.localNum + 1);
                    // 65535 should be enough for anyone
                    if (!s.localNum)
                    {
                        e.error("more than 65535 symbols with name `%s` generated", s.ident.toChars());
                        return setError();
                    }

                    // Replace originalSymbol with s, which updates the localCount
                    sc.func.localsymtab.update(s);

                    // The mangling change only works for D mangling
                }

                if (!(sc.flags & SCOPE.Cfile))
                {
                    /* https://issues.dlang.org/show_bug.cgi?id=21272
                     * If we are in a foreach body we need to extract the
                     * function containing the foreach
                     */
                    FuncDeclaration fes_enclosing_func;
                    if (sc.func && sc.func.fes)
                        fes_enclosing_func = sc.enclosing.enclosing.func;

                    // Disallow shadowing
                    for (Scope* scx = sc.enclosing; scx && (scx.func == sc.func || (fes_enclosing_func && scx.func == fes_enclosing_func)); scx = scx.enclosing)
                    {
                        Dsymbol s2;
                        if (scx.scopesym && scx.scopesym.symtab && (s2 = scx.scopesym.symtab.lookup(s.ident)) !is null && s != s2)
                        {
                            // allow STC.local symbols to be shadowed
                            // TODO: not really an optimal design
                            auto decl = s2.isDeclaration();
                            if (!decl || !(decl.storage_class & STC.local))
                            {
                                if (sc.func.fes)
                                {
                                    e.deprecation("%s `%s` is shadowing %s `%s`. Rename the `foreach` variable.", s.kind(), s.ident.toChars(), s2.kind(), s2.toPrettyChars());
                                }
                                else
                                {
                                    e.error("%s `%s` is shadowing %s `%s`", s.kind(), s.ident.toChars(), s2.kind(), s2.toPrettyChars());
                                    return setError();
                                }
                            }
                        }
                    }
                }
            }
        }
        if (!s.isVarDeclaration())
        {
            Scope* sc2 = sc;
            if (sc2.stc & (STC.pure_ | STC.nothrow_ | STC.nogc))
                sc2 = sc.push();
            sc2.stc &= ~(STC.pure_ | STC.nothrow_ | STC.nogc);
            e.declaration.dsymbolSemantic(sc2);
            if (sc2 != sc)
                sc2.pop();
            s.parent = sc.parent;
        }
        if (global.errors == olderrors)
        {
            e.declaration.semantic2(sc);
            if (global.errors == olderrors)
            {
                e.declaration.semantic3(sc);
            }
        }
        // todo: error in declaration should be propagated.

        e.type = Type.tvoid;
        result = e;
    }

    override void visit(TypeidExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("TypeidExp::semantic() %s\n", exp.toChars());
        }
        Type ta = isType(exp.obj);
        Expression ea = isExpression(exp.obj);
        Dsymbol sa = isDsymbol(exp.obj);
        //printf("ta %p ea %p sa %p\n", ta, ea, sa);

        if (ta)
        {
            dmd.typesem.resolve(ta, exp.loc, sc, ea, ta, sa, true);
        }

        if (ea)
        {
            if (auto sym = getDsymbol(ea))
                ea = symbolToExp(sym, exp.loc, sc, false);
            else
                ea = ea.expressionSemantic(sc);
            ea = resolveProperties(sc, ea);
            ta = ea.type;
            if (ea.op == EXP.type)
                ea = null;
        }

        if (!ta)
        {
            //printf("ta %p ea %p sa %p\n", ta, ea, sa);
            exp.error("no type for `typeid(%s)`", ea ? ea.toChars() : (sa ? sa.toChars() : ""));
            return setError();
        }

        ta.checkComplexTransition(exp.loc, sc);

        Expression e;
        auto tb = ta.toBasetype();
        if (ea && tb.ty == Tclass)
        {
            if (tb.toDsymbol(sc).isClassDeclaration().classKind == ClassKind.cpp)
            {
                error(exp.loc, "runtime type information is not supported for `extern(C++)` classes");
                e = ErrorExp.get();
            }
            else if (!Type.typeinfoclass)
            {
                error(exp.loc, "`object.TypeInfo_Class` could not be found, but is implicitly used");
                e = ErrorExp.get();
            }
            else
            {
                /* Get the dynamic type, which is .classinfo
                */
                ea = ea.expressionSemantic(sc);
                e = new TypeidExp(ea.loc, ea);
                e.type = Type.typeinfoclass.type;
            }
        }
        else if (ta.ty == Terror)
        {
            e = ErrorExp.get();
        }
        else
        {
            // Handle this in the glue layer
            e = new TypeidExp(exp.loc, ta);

            bool genObjCode = true;

            // https://issues.dlang.org/show_bug.cgi?id=23650
            // We generate object code for typeinfo, required
            // by typeid, only if in non-speculative context
            if (sc.flags & SCOPE.compile)
            {
                genObjCode = false;
            }

            e.type = getTypeInfoType(exp.loc, ta, sc, genObjCode);
            semanticTypeInfo(sc, ta);

            if (ea)
            {
                e = new CommaExp(exp.loc, ea, e); // execute ea
                e = e.expressionSemantic(sc);
            }
        }
        result = e;
    }

    override void visit(TraitsExp e)
    {
        result = semanticTraits(e, sc);
    }

    override void visit(HaltExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("HaltExp::semantic()\n");
        }
        e.type = Type.tnoreturn;
        result = e;
    }

    override void visit(IsExp e)
    {
        /* is(targ id tok tspec)
         * is(targ id :  tok2)
         * is(targ id == tok2)
         */
        Type tded = null;

        void yes()
        {
            //printf("yes\n");
            if (!e.id)
            {
                result = IntegerExp.createBool(true);
                return;
            }

            Dsymbol s;
            Tuple tup = isTuple(tded);
            if (tup)
                s = new TupleDeclaration(e.loc, e.id, &tup.objects);
            else
                s = new AliasDeclaration(e.loc, e.id, tded);
            s.dsymbolSemantic(sc);

            /* The reason for the !tup is unclear. It fails Phobos unittests if it is not there.
             * More investigation is needed.
             */
            if (!tup && !sc.insert(s))
            {
                auto conflict = sc.search(Loc.initial, s.ident, null);
                e.error("declaration `%s` is already defined", s.toPrettyChars());
                errorSupplemental(conflict.loc, "`%s` `%s` is defined here",
                                  conflict.kind(), conflict.toChars());
            }

            unSpeculative(sc, s);

            result = IntegerExp.createBool(true);
        }
        void no()
        {
            result = IntegerExp.createBool(false);
            //printf("no\n");
        }

        static if (LOGSEMANTIC)
        {
            printf("IsExp::semantic(%s)\n", e.toChars());
        }
        if (e.id && !(sc.flags & SCOPE.condition))
        {
            e.error("can only declare type aliases within `static if` conditionals or `static assert`s");
            return setError();
        }

        if (e.tok2 == TOK.package_ || e.tok2 == TOK.module_) // These is() expressions are special because they can work on modules, not just types.
        {
            const oldErrors = global.startGagging();
            Dsymbol sym = e.targ.toDsymbol(sc);
            global.endGagging(oldErrors);

            if (sym is null)
                return no();
            Package p = resolveIsPackage(sym);
            if (p is null)
                return no();
            if (e.tok2 == TOK.package_ && p.isModule()) // Note that isModule() will return null for package modules because they're not actually instances of Module.
                return no();
            else if(e.tok2 == TOK.module_ && !(p.isModule() || p.isPackageMod()))
                return no();
            tded = e.targ;
            return yes();
        }

        {
            Scope* sc2 = sc.copy(); // keep sc.flags
            sc2.tinst = null;
            sc2.minst = null;
            sc2.flags |= SCOPE.fullinst;
            Type t = e.targ.trySemantic(e.loc, sc2);
            sc2.pop();
            if (!t) // errors, so condition is false
                return no();
            e.targ = t;
        }

        if (e.tok2 != TOK.reserved)
        {
            switch (e.tok2)
            {
            case TOK.struct_:
                if (e.targ.ty != Tstruct)
                    return no();
                if ((cast(TypeStruct)e.targ).sym.isUnionDeclaration())
                    return no();
                tded = e.targ;
                break;

            case TOK.union_:
                if (e.targ.ty != Tstruct)
                    return no();
                if (!(cast(TypeStruct)e.targ).sym.isUnionDeclaration())
                    return no();
                tded = e.targ;
                break;

            case TOK.class_:
                if (e.targ.ty != Tclass)
                    return no();
                if ((cast(TypeClass)e.targ).sym.isInterfaceDeclaration())
                    return no();
                tded = e.targ;
                break;

            case TOK.interface_:
                if (e.targ.ty != Tclass)
                    return no();
                if (!(cast(TypeClass)e.targ).sym.isInterfaceDeclaration())
                    return no();
                tded = e.targ;
                break;

            case TOK.const_:
                if (!e.targ.isConst())
                    return no();
                tded = e.targ;
                break;

            case TOK.immutable_:
                if (!e.targ.isImmutable())
                    return no();
                tded = e.targ;
                break;

            case TOK.shared_:
                if (!e.targ.isShared())
                    return no();
                tded = e.targ;
                break;

            case TOK.inout_:
                if (!e.targ.isWild())
                    return no();
                tded = e.targ;
                break;

            case TOK.super_:
                // If class or interface, get the base class and interfaces
                if (e.targ.ty != Tclass)
                    return no();
                else
                {
                    ClassDeclaration cd = (cast(TypeClass)e.targ).sym;
                    auto args = new Parameters();
                    args.reserve(cd.baseclasses.length);
                    if (cd.semanticRun < PASS.semanticdone)
                        cd.dsymbolSemantic(null);
                    for (size_t i = 0; i < cd.baseclasses.length; i++)
                    {
                        BaseClass* b = (*cd.baseclasses)[i];
                        args.push(new Parameter(STC.in_, b.type, null, null, null));
                    }
                    tded = new TypeTuple(args);
                }
                break;

            case TOK.enum_:
                if (e.targ.ty != Tenum)
                    return no();
                if (e.id)
                    tded = (cast(TypeEnum)e.targ).sym.getMemtype(e.loc);
                else
                    tded = e.targ;

                if (tded.ty == Terror)
                    return setError();
                break;

            case TOK.delegate_:
                if (e.targ.ty != Tdelegate)
                    return no();
                tded = (cast(TypeDelegate)e.targ).next; // the underlying function type
                break;

            case TOK.function_:
            case TOK.parameters:
                {
                    if (e.targ.ty != Tfunction)
                        return no();
                    tded = e.targ;

                    /* Generate tuple from function parameter types.
                     */
                    assert(tded.ty == Tfunction);
                    auto tdedf = tded.isTypeFunction();
                    auto args = new Parameters();
                    foreach (i, arg; tdedf.parameterList)
                    {
                        assert(arg && arg.type);
                        /* If one of the default arguments was an error,
                           don't return an invalid tuple
                         */
                        if (e.tok2 == TOK.parameters && arg.defaultArg && arg.defaultArg.op == EXP.error)
                            return setError();
                        args.push(new Parameter(arg.storageClass, arg.type, (e.tok2 == TOK.parameters) ? arg.ident : null, (e.tok2 == TOK.parameters) ? arg.defaultArg : null, arg.userAttribDecl));
                    }
                    tded = new TypeTuple(args);
                    break;
                }
            case TOK.return_:
                /* Get the 'return type' for the function,
                 * delegate, or pointer to function.
                 */
                if (auto tf = e.targ.isFunction_Delegate_PtrToFunction())
                    tded = tf.next;
                else
                    return no();
                break;

            case TOK.argumentTypes:
                /* Generate a type tuple of the equivalent types used to determine if a
                 * function argument of this type can be passed in registers.
                 * The results of this are highly platform dependent, and intended
                 * primarly for use in implementing va_arg().
                 */
                tded = target.toArgTypes(e.targ);
                if (!tded)
                    return no();
                // not valid for a parameter
                break;

            case TOK.vector:
                if (e.targ.ty != Tvector)
                    return no();
                tded = (cast(TypeVector)e.targ).basetype;
                break;

            default:
                assert(0);
            }

            // https://issues.dlang.org/show_bug.cgi?id=18753
            if (tded)
                return yes();
            return no();
        }
        else if (e.tspec && !e.id && !(e.parameters && e.parameters.length))
        {
            /* Evaluate to true if targ matches tspec
             * is(targ == tspec)
             * is(targ : tspec)
             */
            e.tspec = e.tspec.typeSemantic(e.loc, sc);
            //printf("targ  = %s, %s\n", e.targ.toChars(), e.targ.deco);
            //printf("tspec = %s, %s\n", e.tspec.toChars(), e.tspec.deco);

            if (e.tok == TOK.colon)
            {
                // current scope is itself deprecated, or deprecations are not errors
                const bool deprecationAllowed = sc.isDeprecated
                    || global.params.useDeprecated != DiagnosticReporting.error;
                const bool preventAliasThis = e.targ.hasDeprecatedAliasThis && !deprecationAllowed;

                if (preventAliasThis && e.targ.ty == Tstruct)
                {
                    if ((cast(TypeStruct) e.targ).implicitConvToWithoutAliasThis(e.tspec))
                        return yes();
                    else
                        return no();
                }
                else if (preventAliasThis && e.targ.ty == Tclass)
                {
                    if ((cast(TypeClass) e.targ).implicitConvToWithoutAliasThis(e.tspec))
                        return yes();
                    else
                        return no();
                }
                else if (e.targ.implicitConvTo(e.tspec))
                    return yes();
                else
                    return no();
            }
            else /* == */
            {
                if (e.targ.equals(e.tspec))
                    return yes();
                else
                    return no();
            }
        }
        else if (e.tspec)
        {
            /* Evaluate to true if targ matches tspec.
             * If true, declare id as an alias for the specialized type.
             * is(targ == tspec, tpl)
             * is(targ : tspec, tpl)
             * is(targ id == tspec)
             * is(targ id : tspec)
             * is(targ id == tspec, tpl)
             * is(targ id : tspec, tpl)
             */
            Identifier tid = e.id ? e.id : Identifier.generateId("__isexp_id");
            e.parameters.insert(0, new TemplateTypeParameter(e.loc, tid, null, null));

            Objects dedtypes = Objects(e.parameters.length);
            dedtypes.zero();

            MATCH m = deduceType(e.targ, sc, e.tspec, e.parameters, &dedtypes, null, 0, e.tok == TOK.equal);

            if (m == MATCH.nomatch || (m != MATCH.exact && e.tok == TOK.equal))
            {
                return no();
            }
            else
            {
                tded = cast(Type)dedtypes[0];
                if (!tded)
                    tded = e.targ;
                Objects tiargs = Objects(1);
                tiargs[0] = e.targ;

                /* Declare trailing parameters
                 */
                for (size_t i = 1; i < e.parameters.length; i++)
                {
                    TemplateParameter tp = (*e.parameters)[i];
                    Declaration s = null;

                    m = tp.matchArg(e.loc, sc, &tiargs, i, e.parameters, &dedtypes, &s);
                    if (m == MATCH.nomatch)
                        return no();
                    s.dsymbolSemantic(sc);
                    if (!sc.insert(s))
                    {
                        auto conflict = sc.search(Loc.initial, s.ident, null);
                        e.error("declaration `%s` is already defined", s.toPrettyChars());
                        errorSupplemental(conflict.loc, "`%s` `%s` is defined here",
                                          conflict.kind(), conflict.toChars());
                    }

                    unSpeculative(sc, s);
                }
                return yes();
            }
        }
        else if (e.id)
        {
            /* Declare id as an alias for type targ. Evaluate to true
             * is(targ id)
             */
            tded = e.targ;
        }
        return yes();
    }

    override void visit(BinAssignExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.e1.op == EXP.arrayLength)
        {
            // arr.length op= e2;
            e = rewriteOpAssign(exp);
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }
        if (exp.e1.op == EXP.slice || exp.e1.type.ty == Tarray || exp.e1.type.ty == Tsarray)
        {
            if (checkNonAssignmentArrayOp(exp.e1))
                return setError();

            if (exp.e1.op == EXP.slice)
                (cast(SliceExp)exp.e1).arrayop = true;

            // T[] op= ...
            if (exp.e2.implicitConvTo(exp.e1.type.nextOf()))
            {
                // T[] op= T
                exp.e2 = exp.e2.castTo(sc, exp.e1.type.nextOf());
            }
            else if (Expression ex = typeCombine(exp, sc))
            {
                result = ex;
                return;
            }
            exp.type = exp.e1.type;
            result = arrayOp(exp, sc);
            return;
        }

        exp.e1 = exp.e1.expressionSemantic(sc);
        exp.e1 = exp.e1.modifiableLvalue(sc, exp.e1);
        exp.e1 = exp.e1.optimize(WANTvalue, /*keepLvalue*/ true);
        exp.type = exp.e1.type;

        if (auto ad = isAggregate(exp.e1.type))
        {
            if (const s = search_function(ad, Id.opOpAssign))
            {
                error(exp.loc, "none of the `opOpAssign` overloads of `%s` are callable for `%s` of type `%s`", ad.toChars(), exp.e1.toChars(), exp.e1.type.toChars());
                return setError();
            }
        }
        if (exp.e1.checkScalar() ||
            exp.e1.checkReadModifyWrite(exp.op, exp.e2) ||
            exp.e1.checkSharedAccess(sc))
            return setError();

        int arith = (exp.op == EXP.addAssign || exp.op == EXP.minAssign || exp.op == EXP.mulAssign || exp.op == EXP.divAssign || exp.op == EXP.modAssign || exp.op == EXP.powAssign);
        int bitwise = (exp.op == EXP.andAssign || exp.op == EXP.orAssign || exp.op == EXP.xorAssign);
        int shift = (exp.op == EXP.leftShiftAssign || exp.op == EXP.rightShiftAssign || exp.op == EXP.unsignedRightShiftAssign);

        if (bitwise && exp.type.toBasetype().ty == Tbool)
            exp.e2 = exp.e2.implicitCastTo(sc, exp.type);
        else if (exp.checkNoBool())
            return setError();

        if ((exp.op == EXP.addAssign || exp.op == EXP.minAssign) && exp.e1.type.toBasetype().ty == Tpointer && exp.e2.type.toBasetype().isintegral())
        {
            result = scaleFactor(exp, sc);
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        if (arith && (exp.checkArithmeticBin() || exp.checkSharedAccessBin(sc)))
            return setError();
        if ((bitwise || shift) && (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc)))
            return setError();

        if (shift)
        {
            if (exp.e2.type.toBasetype().ty != Tvector)
                exp.e2 = exp.e2.castTo(sc, Type.tshiftcnt);
        }

        if (!target.isVectorOpSupported(exp.type.toBasetype(), exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }

        if (exp.e1.op == EXP.error || exp.e2.op == EXP.error)
            return setError();

        e = exp.checkOpAssignTypes(sc);
        if (e.op == EXP.error)
        {
            result = e;
            return;
        }

        assert(e.op == EXP.assign || e == exp);
        result = (cast(BinExp)e).reorderSettingAAElem(sc);
    }

    private Expression compileIt(MixinExp exp)
    {
        OutBuffer buf;
        if (expressionsToString(buf, sc, exp.exps))
            return null;

        uint errors = global.errors;
        const len = buf.length;
        const str = buf.extractChars()[0 .. len];
        scope p = new Parser!ASTCodegen(exp.loc, sc._module, str, false, global.errorSink);
        p.nextToken();
        //printf("p.loc.linnum = %d\n", p.loc.linnum);

        Expression e = p.parseExpression();
        if (global.errors != errors)
            return null;

        if (p.token.value != TOK.endOfFile)
        {
            exp.error("incomplete mixin expression `%s`", str.ptr);
            return null;
        }
        return e;
    }

    override void visit(MixinExp exp)
    {
        /* https://dlang.org/spec/expression.html#mixin_expressions
         */

        static if (LOGSEMANTIC)
        {
            printf("MixinExp::semantic('%s')\n", exp.toChars());
        }

        auto e = compileIt(exp);
        if (!e)
            return setError();
        result = e.expressionSemantic(sc);
    }

    override void visit(ImportExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("ImportExp::semantic('%s')\n", e.toChars());
        }

        auto se = semanticString(sc, e.e1, "file name argument");
        if (!se)
            return setError();
        se = se.toUTF8(sc);

        auto namez = se.toStringz();
        if (!global.filePath)
        {
            e.error("need `-J` switch to import text file `%s`", namez.ptr);
            return setError();
        }

        /* Be wary of CWE-22: Improper Limitation of a Pathname to a Restricted Directory
         * ('Path Traversal') attacks.
         * https://cwe.mitre.org/data/definitions/22.html
         */

        if (FileName.absolute(namez))
        {
            e.error("absolute path is not allowed in import expression: `%s`", se.toChars());
            return setError();
        }

        auto idxReserved = FileName.findReservedChar(namez);
        if (idxReserved != size_t.max)
        {
            e.error("`%s` is not a valid filename on this platform", se.toChars());
            e.errorSupplemental("Character `'%c'` is reserved and cannot be used", namez[idxReserved]);
            return setError();
        }

        if (FileName.refersToParentDir(namez))
        {
            e.error("path refers to parent (`..`) directory: `%s`", se.toChars());
            return setError();
        }

        auto resolvedNamez = FileName.searchPath(global.filePath, namez, false);
        if (!resolvedNamez)
        {
            e.error("file `%s` cannot be found or not in a path specified with `-J`", se.toChars());
            e.errorSupplemental("Path(s) searched (as provided by `-J`):");
            foreach (idx, path; *global.filePath)
            {
                const attr = FileName.exists(path);
                const(char)* err = attr == 2 ? "" :
                    (attr == 1 ? " (not a directory)" : " (path not found)");
                e.errorSupplemental("[%llu]: `%s`%s", cast(ulong)idx, path, err);
            }
            return setError();
        }

        sc._module.contentImportedFiles.push(resolvedNamez.ptr);
        if (global.params.verbose)
        {
            const slice = se.peekString();
            message("file      %.*s\t(%s)", cast(int)slice.length, slice.ptr, resolvedNamez.ptr);
        }
        if (global.params.moduleDeps.buffer !is null)
        {
            OutBuffer* ob = global.params.moduleDeps.buffer;
            Module imod = sc._module;

            if (!global.params.moduleDeps.name)
                ob.writestring("depsFile ");
            ob.writestring(imod.toPrettyChars());
            ob.writestring(" (");
            escapePath(ob, imod.srcfile.toChars());
            ob.writestring(") : ");
            if (global.params.moduleDeps.name)
                ob.writestring("string : ");
            ob.write(se.peekString());
            ob.writestring(" (");
            escapePath(ob, resolvedNamez.ptr);
            ob.writestring(")");
            ob.writenl();
        }
        if (global.params.makeDeps.doOutput)
        {
            global.params.makeDeps.files.push(resolvedNamez.ptr);
        }

        {
            auto fileName = FileName(resolvedNamez);
            if (auto fmResult = global.fileManager.lookup(fileName))
            {
                se = new StringExp(e.loc, fmResult);
            }
            else
            {
                auto readResult = File.read(resolvedNamez);
                if (!readResult.success)
                {
                    e.error("cannot read file `%s`", resolvedNamez.ptr);
                    return setError();
                }
                else
                {
                    // take ownership of buffer (probably leaking)
                    auto data = readResult.extractSlice();
                    se = new StringExp(e.loc, data);
                    global.fileManager.add(fileName, data);
                }
            }
        }
        result = se.expressionSemantic(sc);
    }

    override void visit(AssertExp exp)
    {
        // https://dlang.org/spec/expression.html#assert_expressions
        static if (LOGSEMANTIC)
        {
            printf("AssertExp::semantic('%s')\n", exp.toChars());
        }

        const generateMsg = !exp.msg && global.params.checkAction == CHECKACTION.context && global.params.useAssert == CHECKENABLE.on;
        Expression temporariesPrefix;

        if (generateMsg)
        // no message - use assert expression as msg
        {
            if (!verifyHookExist(exp.loc, *sc, Id._d_assert_fail, "generating assert messages"))
                return setError();

            /*
            {
              auto a = e1, b = e2;
              assert(a == b, _d_assert_fail!"=="(a, b));
            }()
            */

            /*
            Stores the result of an operand expression into a temporary
            if necessary, e.g. if it is an impure fuction call containing side
            effects as in https://issues.dlang.org/show_bug.cgi?id=20114

            Params:
                op = an expression which may require a temporary (added to
                     `temporariesPrefix`: `auto tmp = op`) and will be replaced
                     by `tmp` if necessary

            Returns: (possibly replaced) `op`
            */
            Expression maybePromoteToTmp(ref Expression op)
            {
                // https://issues.dlang.org/show_bug.cgi?id=20989
                // Flag that _d_assert_fail will never dereference `array.ptr` to avoid safety
                // errors for `assert(!array.ptr)` => `_d_assert_fail!"!"(array.ptr)`
                {
                    auto die = op.isDotIdExp();
                    if (die && die.ident == Id.ptr)
                        die.noderef = true;
                }

                op = op.expressionSemantic(sc);
                op = resolveProperties(sc, op);

                // Detect assert's using static operator overloads (e.g. `"var" in environment`)
                if (auto te = op.isTypeExp())
                {
                    // Replace the TypeExp with it's textual representation
                    // Including "..." in the error message isn't quite right but
                    // proper solutions require more drastic changes, e.g. directly
                    // using miniFormat and combine instead of calling _d_assert_fail
                    auto name = new StringExp(te.loc, te.toString());
                    return name.expressionSemantic(sc);
                }

                // Create a temporary for expressions with side effects
                // Defensively assume that function calls may have side effects even
                // though it's not detected by hasSideEffect (e.g. `debug puts("Hello")` )
                // Rewriting CallExp's also avoids some issues with the inliner/debug generation
                if (op.hasSideEffect(true))
                {
                    // Don't create an invalid temporary for void-expressions
                    // Further semantic will issue an appropriate error
                    if (op.type.ty == Tvoid)
                        return op;

                    // https://issues.dlang.org/show_bug.cgi?id=21590
                    // Don't create unnecessary temporaries and detect `assert(a = b)`
                    if (op.isAssignExp() || op.isBinAssignExp())
                    {
                        auto left = (cast(BinExp) op).e1;

                        // Find leftmost expression to handle other rewrites,
                        // e.g. --(++a) => a += 1 -= 1
                        while (left.isAssignExp() || left.isBinAssignExp())
                            left = (cast(BinExp) left).e1;

                        // Only use the assignee if it's a variable and skip
                        // other lvalues (e.g. ref's returned by functions)
                        if (left.isVarExp())
                            return left;

                        // Sanity check that `op` can be converted to boolean
                        // But don't raise errors for assignments enclosed in another expression
                        if (op is exp.e1)
                            op.toBoolean(sc);
                    }

                    // Tuples with side-effects already receive a temporary during semantic
                    if (op.type.isTypeTuple())
                    {
                        auto te = op.isTupleExp();
                        assert(te);

                        // Create a new tuple without the associated temporary
                        auto res = new TupleExp(op.loc, te.exps);
                        return res.expressionSemantic(sc);
                    }

                    const stc = op.isLvalue() ? STC.ref_ : 0;
                    auto tmp = copyToTemp(stc, "__assertOp", op);
                    tmp.dsymbolSemantic(sc);

                    auto decl = new DeclarationExp(op.loc, tmp);
                    temporariesPrefix = Expression.combine(temporariesPrefix, decl);

                    op = new VarExp(op.loc, tmp);
                    op = op.expressionSemantic(sc);
                }
                return op;
            }

            // if the assert condition is a mixin expression, try to compile it
            if (auto ce = exp.e1.isMixinExp())
            {
                if (auto e1 = compileIt(ce))
                    exp.e1 = e1;
            }

            Expressions* es;
            Objects* tiargs;
            Loc loc = exp.e1.loc;

            const op = exp.e1.op;
            bool isEqualsCallExpression;
            if (const callExp = exp.e1.isCallExp())
            {
                // https://issues.dlang.org/show_bug.cgi?id=20331
                // callExp.f may be null if the assert contains a call to
                // a function pointer or literal
                if (const callExpFunc = callExp.f)
                {
                    const callExpIdent = callExpFunc.ident;
                    isEqualsCallExpression = callExpIdent == Id.__equals ||
                                             callExpIdent == Id.eq;
                }
            }
            if (op == EXP.equal || op == EXP.notEqual ||
                op == EXP.lessThan || op == EXP.greaterThan ||
                op == EXP.lessOrEqual || op == EXP.greaterOrEqual ||
                op == EXP.identity || op == EXP.notIdentity ||
                op == EXP.in_ ||
                isEqualsCallExpression)
            {
                es = new Expressions(3);
                tiargs = new Objects(1);

                if (isEqualsCallExpression)
                {
                    auto callExp = cast(CallExp) exp.e1;
                    auto args = callExp.arguments;

                    // structs with opEquals get rewritten to a DotVarExp:
                    // a.opEquals(b)
                    // https://issues.dlang.org/show_bug.cgi?id=20100
                    if (args.length == 1)
                    {
                        auto dv = callExp.e1.isDotVarExp();
                        assert(dv);

                        // runtime args
                        (*es)[1] = maybePromoteToTmp(dv.e1);
                        (*es)[2] = maybePromoteToTmp((*args)[0]);
                    }
                    else
                    {
                        // runtime args
                        (*es)[1] = maybePromoteToTmp((*args)[0]);
                        (*es)[2] = maybePromoteToTmp((*args)[1]);
                    }
                }
                else
                {
                    auto binExp = cast(EqualExp) exp.e1;

                    // runtime args
                    (*es)[1] = maybePromoteToTmp(binExp.e1);
                    (*es)[2] = maybePromoteToTmp(binExp.e2);
                }

                // template args
                Expression comp = new StringExp(loc, isEqualsCallExpression ? "==" : EXPtoString(exp.e1.op));
                comp = comp.expressionSemantic(sc);
                (*es)[0] = comp;
                (*tiargs)[0] = (*es)[1].type;
            }

            // Format exp.e1 before any additional boolean conversion
            // Ignore &&/|| because "assert(...) failed" is more informative than "false != true"
            else if (op != EXP.andAnd && op != EXP.orOr)
            {
                es = new Expressions(2);
                tiargs = new Objects(1);

                if (auto ne = exp.e1.isNotExp())
                {
                    // Fetch the (potential non-bool) expression and fold
                    // (n) negations into (n % 2) negations, e.g. !!a => a
                    for (bool neg = true; ; neg = !neg)
                    {
                        if (auto ne2 = ne.e1.isNotExp())
                            ne = ne2;
                        else
                        {
                            (*es)[0] = new StringExp(loc, neg ? "!" : "");
                            (*es)[1] = maybePromoteToTmp(ne.e1);
                            break;
                        }
                    }
                }
                else
                {   // Simply format exp.e1
                    (*es)[0] = new StringExp(loc, "");
                    (*es)[1] = maybePromoteToTmp(exp.e1);
                }

                (*tiargs)[0] = (*es)[1].type;

                // Passing __ctfe to auto ref infers ref and aborts compilation:
                // "cannot modify compiler-generated variable __ctfe"
                auto ve = (*es)[1].isVarExp();
                if (ve && ve.var.ident == Id.ctfe)
                {
                    exp.msg = new StringExp(loc, "assert(__ctfe) failed!");
                    goto LSkip;
                }
            }
            else
            {
                OutBuffer buf;
                buf.printf("%s failed", exp.toChars());
                exp.msg = new StringExp(Loc.initial, buf.extractSlice());
                goto LSkip;
            }

            Expression __assertFail = new IdentifierExp(exp.loc, Id.empty);
            auto assertFail = new DotIdExp(loc, __assertFail, Id.object);

            auto dt = new DotTemplateInstanceExp(loc, assertFail, Id._d_assert_fail, tiargs);
            auto ec = CallExp.create(loc, dt, es);
            exp.msg = ec;
        }

        LSkip:
        if (Expression ex = unaSemantic(exp, sc))
        {
            result = ex;
            return;
        }

        exp.e1 = resolveProperties(sc, exp.e1);
        // BUG: see if we can do compile time elimination of the Assert
        exp.e1 = exp.e1.optimize(WANTvalue);
        exp.e1 = exp.e1.toBoolean(sc);

        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }

        if (exp.msg)
        {
            exp.msg = expressionSemantic(exp.msg, sc);
            exp.msg = resolveProperties(sc, exp.msg);
            exp.msg = exp.msg.implicitCastTo(sc, Type.tchar.constOf().arrayOf());
            exp.msg = exp.msg.optimize(WANTvalue);
            checkParamArgumentEscape(sc, null, null, null, STC.undefined_, exp.msg, true, false);
        }

        if (exp.msg && exp.msg.op == EXP.error)
        {
            result = exp.msg;
            return;
        }

        auto f1 = checkNonAssignmentArrayOp(exp.e1);
        auto f2 = exp.msg && checkNonAssignmentArrayOp(exp.msg);
        if (f1 || f2)
            return setError();

        if (exp.e1.toBool().hasValue(false))
        {
            /* This is an `assert(0)` which means halt program execution
             */
            FuncDeclaration fd = sc.parent.isFuncDeclaration();
            if (fd)
                fd.hasReturnExp |= 4;
            sc.ctorflow.orCSX(CSX.halt);

            if (global.params.useAssert == CHECKENABLE.off)
            {
                Expression e = new HaltExp(exp.loc);
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }

            // Only override the type when it isn't already some flavour of noreturn,
            // e.g. when this assert was generated by defaultInitLiteral
            if (!exp.type || !exp.type.isTypeNoreturn())
                exp.type = Type.tnoreturn;
        }
        else
            exp.type = Type.tvoid;

        result = !temporariesPrefix
            ? exp
            : Expression.combine(temporariesPrefix, exp).expressionSemantic(sc);
    }

    override void visit(ThrowExp te)
    {
        import dmd.statementsem;

        if (StatementSemanticVisitor.throwSemantic(te.loc, te.e1, sc))
            result = te;
        else
            setError();
    }

    override void visit(DotIdExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotIdExp::semantic(this = %p, '%s')\n", exp, exp.toChars());
            //printf("e1.op = %d, '%s'\n", e1.op, Token.toChars(e1.op));
        }

        if (sc.flags & SCOPE.Cfile)
        {
            /* See if need to rewrite the AST because of cast/call ambiguity
             */
            if (auto e = castCallAmbiguity(exp, sc))
            {
                result = expressionSemantic(e, sc);
                return;
            }

            if (exp.arrow) // ImportC only
                exp.e1 = exp.e1.expressionSemantic(sc).arrayFuncConv(sc);

            if (exp.ident == Id.__xalignof && exp.e1.isTypeExp())
            {
                // C11 6.5.3 says _Alignof only applies to types
                Expression e;
                Type t;
                Dsymbol s;
                dmd.typesem.resolve(exp.e1.type, exp.e1.loc, sc, e, t, s, true);
                if (e)
                {
                    exp.e1.error("argument to `_Alignof` must be a type");
                    return setError();
                }
                else if (t)
                {
                    // Note similarity to getProperty() implementation of __xalignof
                    const explicitAlignment = t.alignment();
                    const naturalAlignment = t.alignsize();
                    const actualAlignment = (explicitAlignment.isDefault() ? naturalAlignment : explicitAlignment.get());
                    result = new IntegerExp(exp.loc, actualAlignment, Type.tsize_t);
                }
                else if (s)
                {
                    exp.e1.error("argument to `_Alignof` must be a type");
                    return setError();
                }
                else
                    assert(0);
                return;
            }

            if (exp.ident != Id.__sizeof)
            {
                result = fieldLookup(exp.e1, sc, exp.ident);
                return;
            }
        }

        Expression e = exp.dotIdSemanticProp(sc, 1);

        if (e && isDotOpDispatch(e))
        {
            auto ode = e;
            uint errors = global.startGagging();
            e = resolvePropertiesX(sc, e);
            // Any error or if 'e' is not resolved, go to UFCS
            if (global.endGagging(errors) || e is ode)
                e = null; /* fall down to UFCS */
            else
            {
                result = e;
                return;
            }
        }
        if (!e) // if failed to find the property
        {
            /* If ident is not a valid property, rewrite:
             *   e1.ident
             * as:
             *   .ident(e1)
             */
            e = resolveUFCSProperties(sc, exp);
        }
        result = e;
    }

    override void visit(DotTemplateExp e)
    {
        if (e.type)
        {
            result = e;
            return;
        }
        if (Expression ex = unaSemantic(e, sc))
        {
            result = ex;
            return;
        }
        // 'void' like TemplateExp
        e.type = Type.tvoid;
        result = e;
    }

    override void visit(DotVarExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotVarExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        exp.var = exp.var.toAlias().isDeclaration();

        exp.e1 = exp.e1.expressionSemantic(sc);

        if (auto tup = exp.var.isTupleDeclaration())
        {
            /* Replace:
             *  e1.tuple(a, b, c)
             * with:
             *  tuple(e1.a, e1.b, e1.c)
             */
            Expression e0;
            Expression ev = sc.func ? extractSideEffect(sc, "__tup", e0, exp.e1) : exp.e1;

            auto exps = new Expressions();
            exps.reserve(tup.objects.length);
            for (size_t i = 0; i < tup.objects.length; i++)
            {
                RootObject o = (*tup.objects)[i];
                Expression e;
                Declaration var;
                switch (o.dyncast()) with (DYNCAST)
                {
                case expression:
                    e = cast(Expression)o;
                    if (auto se = e.isDsymbolExp())
                        var = se.s.isDeclaration();
                    else if (auto ve = e.isVarExp())
                        if (!ve.var.isFuncDeclaration())
                            // Exempt functions for backwards compatibility reasons.
                            // See: https://issues.dlang.org/show_bug.cgi?id=20470#c1
                            var = ve.var;
                    break;
                case dsymbol:
                    Dsymbol s = cast(Dsymbol) o;
                    Declaration d = s.isDeclaration();
                    if (!d || d.isFuncDeclaration())
                        // Exempt functions for backwards compatibility reasons.
                        // See: https://issues.dlang.org/show_bug.cgi?id=20470#c1
                        e = new DsymbolExp(exp.loc, s);
                    else
                        var = d;
                    break;
                case type:
                    e = new TypeExp(exp.loc, cast(Type)o);
                    break;
                default:
                    exp.error("`%s` is not an expression", o.toChars());
                    return setError();
                }
                if (var)
                    e = new DotVarExp(exp.loc, ev, var);
                exps.push(e);
            }

            Expression e = new TupleExp(exp.loc, e0, exps);
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }
        else if (auto ad = exp.var.isAliasDeclaration())
        {
            if (auto t = ad.getType())
            {
                result = new TypeExp(exp.loc, t).expressionSemantic(sc);
                return;
            }
        }

        exp.e1 = exp.e1.addDtorHook(sc);

        Type t1 = exp.e1.type;

        if (FuncDeclaration fd = exp.var.isFuncDeclaration())
        {
            // for functions, do checks after overload resolution
            if (!fd.functionSemantic())
                return setError();

            /* https://issues.dlang.org/show_bug.cgi?id=13843
             * If fd obviously has no overloads, we should
             * normalize AST, and it will give a chance to wrap fd with FuncExp.
             */
            if ((fd.isNested() && !fd.isThis()) || fd.isFuncLiteralDeclaration())
            {
                // (e1, fd)
                auto e = symbolToExp(fd, exp.loc, sc, false);
                result = Expression.combine(exp.e1, e);
                return;
            }

            exp.type = fd.type;
            assert(exp.type);
        }
        else if (OverDeclaration od = exp.var.isOverDeclaration())
        {
            exp.type = Type.tvoid; // ambiguous type?
        }
        else
        {
            exp.type = exp.var.type;
            if (!exp.type && global.errors) // var is goofed up, just return error.
                return setError();
            assert(exp.type);

            if (t1.ty == Tpointer)
                t1 = t1.nextOf();

            exp.type = exp.type.addMod(t1.mod);

            // https://issues.dlang.org/show_bug.cgi?id=23109
            // Run semantic on the DotVarExp type
            if (auto handle = exp.type.isClassHandle())
            {
                if (handle.semanticRun < PASS.semanticdone && !handle.isBaseInfoComplete())
                    handle.dsymbolSemantic(null);
            }

            Dsymbol vparent = exp.var.toParent();
            AggregateDeclaration ad = vparent ? vparent.isAggregateDeclaration() : null;
            if (Expression e1x = getRightThis(exp.loc, sc, ad, exp.e1, exp.var, 1))
                exp.e1 = e1x;
            else
            {
                /* Later checkRightThis will report correct error for invalid field variable access.
                 */
                Expression e = new VarExp(exp.loc, exp.var);
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }
            checkAccess(exp.loc, sc, exp.e1, exp.var);

            VarDeclaration v = exp.var.isVarDeclaration();
            if (v && (v.isDataseg() || (v.storage_class & STC.manifest)))
            {
                Expression e = expandVar(WANTvalue, v);
                if (e)
                {
                    result = e;
                    return;
                }
            }

            if (v && (v.isDataseg() || // fix https://issues.dlang.org/show_bug.cgi?id=8238
                      (!v.needThis() && v.semanticRun > PASS.initial)))  // fix https://issues.dlang.org/show_bug.cgi?id=17258
            {
                // (e1, v)
                checkAccess(exp.loc, sc, exp.e1, v);
                Expression e = new VarExp(exp.loc, v);
                e = new CommaExp(exp.loc, exp.e1, e);
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }
        }
        //printf("-DotVarExp::semantic('%s')\n", toChars());
        result = exp;
    }

    override void visit(DotTemplateInstanceExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotTemplateInstanceExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }
        // Indicate we need to resolve by UFCS.
        Expression e = exp.dotTemplateSemanticProp(sc, 1);
        if (!e)
            e = resolveUFCSProperties(sc, exp);
        if (e is exp)
            e.type = Type.tvoid; // Unresolved type, because it needs inference
        result = e;
    }

    override void visit(DelegateExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("DelegateExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        e.e1 = e.e1.expressionSemantic(sc);

        e.type = new TypeDelegate(e.func.type.isTypeFunction());
        e.type = e.type.typeSemantic(e.loc, sc);

        FuncDeclaration f = e.func.toAliasFunc();
        AggregateDeclaration ad = f.isMemberLocal();
        if (f.needThis())
            e.e1 = getRightThis(e.loc, sc, ad, e.e1, f);

        if (f.type.ty == Tfunction)
        {
            TypeFunction tf = cast(TypeFunction)f.type;
            if (!MODmethodConv(e.e1.type.mod, f.type.mod))
            {
                OutBuffer thisBuf, funcBuf;
                MODMatchToBuffer(&thisBuf, e.e1.type.mod, tf.mod);
                MODMatchToBuffer(&funcBuf, tf.mod, e.e1.type.mod);
                e.error("%smethod `%s` is not callable using a %s`%s`",
                    funcBuf.peekChars(), f.toPrettyChars(), thisBuf.peekChars(), e.e1.toChars());
                return setError();
            }
        }
        if (ad && ad.isClassDeclaration() && ad.type != e.e1.type)
        {
            // A downcast is required for interfaces
            // https://issues.dlang.org/show_bug.cgi?id=3706
            e.e1 = new CastExp(e.loc, e.e1, ad.type);
            e.e1 = e.e1.expressionSemantic(sc);
        }
        result = e;
        // declare dual-context container
        if (f.hasDualContext() && !sc.intypeof && sc.func)
        {
            // check access to second `this`
            if (AggregateDeclaration ad2 = f.isMember2())
            {
                Expression te = new ThisExp(e.loc).expressionSemantic(sc);
                if (te.op != EXP.error)
                    te = getRightThis(e.loc, sc, ad2, te, f);
                if (te.op == EXP.error)
                {
                    e.error("need `this` of type `%s` to make delegate from function `%s`", ad2.toChars(), f.toChars());
                    return setError();
                }
            }
            VarDeclaration vthis2 = makeThis2Argument(e.loc, sc, f);
            e.vthis2 = vthis2;
            Expression de = new DeclarationExp(e.loc, vthis2);
            result = Expression.combine(de, result);
            result = result.expressionSemantic(sc);
        }
    }

    override void visit(DotTypeExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotTypeExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (auto e = unaSemantic(exp, sc))
        {
            result = e;
            return;
        }

        exp.type = exp.sym.getType().addMod(exp.e1.type.mod);
        result = exp;
    }

    override void visit(AddrExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("AddrExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = unaSemantic(exp, sc))
        {
            result = ex;
            return;
        }

        if (sc.flags & SCOPE.Cfile)
        {
            /* Special handling for &"string"/&(T[]){0, 1}
             * since C regards string/array literals as lvalues
             */
            auto e = exp.e1;
            if(e.isStringExp() || e.isArrayLiteralExp())
            {
                e.type = typeSemantic(e.type, Loc.initial, sc);
                // if type is already a pointer exp is an illegal expression of the form `&(&"")`
                if (!e.type.isTypePointer())
                {
                    e.type = e.type.pointerTo();
                    result = e;
                    return;
                }
                else
                {
                    // `toLvalue` call further below is upon exp.e1, omitting & from the error message
                    exp.toLvalue(sc, null);
                    return setError();
                }
            }
        }

        int wasCond = exp.e1.op == EXP.question;

        if (exp.e1.op == EXP.dotTemplateInstance)
        {
            DotTemplateInstanceExp dti = cast(DotTemplateInstanceExp)exp.e1;
            TemplateInstance ti = dti.ti;
            {
                //assert(ti.needsTypeInference(sc));
                ti.dsymbolSemantic(sc);
                if (!ti.inst || ti.errors) // if template failed to expand
                    return setError();

                Dsymbol s = ti.toAlias();
                FuncDeclaration f = s.isFuncDeclaration();
                if (f)
                {
                    exp.e1 = new DotVarExp(exp.e1.loc, dti.e1, f);
                    exp.e1 = exp.e1.expressionSemantic(sc);
                }
            }
        }
        else if (exp.e1.op == EXP.scope_)
        {
            TemplateInstance ti = (cast(ScopeExp)exp.e1).sds.isTemplateInstance();
            if (ti)
            {
                //assert(ti.needsTypeInference(sc));
                ti.dsymbolSemantic(sc);
                if (!ti.inst || ti.errors) // if template failed to expand
                    return setError();

                Dsymbol s = ti.toAlias();
                FuncDeclaration f = s.isFuncDeclaration();
                if (f)
                {
                    exp.e1 = new VarExp(exp.e1.loc, f);
                    exp.e1 = exp.e1.expressionSemantic(sc);
                }
            }
        }
        /* https://issues.dlang.org/show_bug.cgi?id=809
         *
         * If the address of a lazy variable is taken,
         * the expression is rewritten so that the type
         * of it is the delegate type. This means that
         * the symbol is not going to represent a call
         * to the delegate anymore, but rather, the
         * actual symbol.
         */
        if (auto ve = exp.e1.isVarExp())
        {
            if (ve.var.storage_class & STC.lazy_)
            {
                exp.e1 = exp.e1.expressionSemantic(sc);
                exp.e1 = resolveProperties(sc, exp.e1);
                if (auto callExp = exp.e1.isCallExp())
                {
                    if (callExp.e1.type.toBasetype().ty == Tdelegate)
                    {
                        /* https://issues.dlang.org/show_bug.cgi?id=20551
                         *
                         * Cannot take address of lazy parameter in @safe code
                         * because it might end up being a pointer to undefined
                         * memory.
                         */
                        if (1)
                        {
                            if (sc.setUnsafe(false, exp.loc,
                                "cannot take address of lazy parameter `%s` in `@safe` function `%s`", ve, sc.func))
                            {
                                setError();
                                return;
                            }
                        }
                        VarExp ve2 = callExp.e1.isVarExp();
                        ve2.delegateWasExtracted = true;
                        ve2.var.storage_class |= STC.scope_;
                        result = ve2;
                        return;
                    }
                }
            }
        }

        exp.e1 = exp.e1.toLvalue(sc, null);
        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        if (checkNonAssignmentArrayOp(exp.e1))
            return setError();

        if (!exp.e1.type)
        {
            exp.error("cannot take address of `%s`", exp.e1.toChars());
            return setError();
        }
        if (!checkAddressable(exp, sc))
            return setError();

        bool hasOverloads;
        if (auto f = isFuncAddress(exp, &hasOverloads))
        {
            if (!hasOverloads && f.checkForwardRef(exp.loc))
                return setError();
        }
        else if (!exp.e1.type.deco)
        {
            // try to resolve the type
            exp.e1.type = exp.e1.type.typeSemantic(exp.e1.loc, null);
            if (!exp.e1.type.deco)  // still couldn't resolve it
            {
                if (auto ve = exp.e1.isVarExp())
                {
                    Declaration d = ve.var;
                    exp.error("forward reference to %s `%s`", d.kind(), d.toChars());
                }
                else
                    exp.error("forward reference to type `%s` of expression `%s`", exp.e1.type.toChars(), exp.e1.toChars());
                return setError();
            }
        }

        exp.type = exp.e1.type.pointerTo();

        // See if this should really be a delegate
        if (exp.e1.op == EXP.dotVariable)
        {
            DotVarExp dve = cast(DotVarExp)exp.e1;
            FuncDeclaration f = dve.var.isFuncDeclaration();
            if (f)
            {
                f = f.toAliasFunc(); // FIXME, should see overloads
                                     // https://issues.dlang.org/show_bug.cgi?id=1983
                if (!dve.hasOverloads)
                    f.tookAddressOf++;

                Expression e;
                if (f.needThis())
                    e = new DelegateExp(exp.loc, dve.e1, f, dve.hasOverloads);
                else // It is a function pointer. Convert &v.f() --> (v, &V.f())
                    e = new CommaExp(exp.loc, dve.e1, new AddrExp(exp.loc, new VarExp(exp.loc, f, dve.hasOverloads)));
                e = e.expressionSemantic(sc);
                result = e;
                return;
            }

            // Look for misaligned pointer in @safe mode
            if (checkUnsafeAccess(sc, dve, !exp.type.isMutable(), true))
                return setError();
        }
        else if (exp.e1.op == EXP.variable)
        {
            VarExp ve = cast(VarExp)exp.e1;
            VarDeclaration v = ve.var.isVarDeclaration();
            if (v)
            {
                if (!checkAddressVar(sc, exp.e1, v))
                    return setError();

                ve.checkPurity(sc, v);
            }
            FuncDeclaration f = ve.var.isFuncDeclaration();
            if (f)
            {
                /* Because nested functions cannot be overloaded,
                 * mark here that we took its address because castTo()
                 * may not be called with an exact match.
                 *
                 * https://issues.dlang.org/show_bug.cgi?id=19285 :
                 * We also need to make sure we aren't inside a typeof. Ideally the compiler
                 * would do typeof(...) semantic analysis speculatively then collect information
                 * about what it used rather than relying on what are effectively semantically-global
                 * variables but it doesn't.
                 */
                if (!sc.isFromSpeculativeSemanticContext() && (!ve.hasOverloads || (f.isNested() && !f.needThis())))
                {
                    // TODO: Refactor to use a proper interface that can keep track of causes.
                    f.tookAddressOf++;
                }

                if (f.isNested() && !f.needThis())
                {
                    if (f.isFuncLiteralDeclaration())
                    {
                        if (!f.FuncDeclaration.isNested())
                        {
                            /* Supply a 'null' for a this pointer if no this is available
                             */
                            Expression e = new DelegateExp(exp.loc, new NullExp(exp.loc, Type.tnull), f, ve.hasOverloads);
                            e = e.expressionSemantic(sc);
                            result = e;
                            return;
                        }
                    }
                    Expression e = new DelegateExp(exp.loc, exp.e1, f, ve.hasOverloads);
                    e = e.expressionSemantic(sc);
                    result = e;
                    return;
                }
                if (f.needThis())
                {
                    auto memberFunc = hasThis(sc);
                    if (memberFunc && haveSameThis(memberFunc, f))
                    {
                        /* Should probably supply 'this' after overload resolution,
                         * not before.
                         */
                        Expression ethis = new ThisExp(exp.loc);
                        Expression e = new DelegateExp(exp.loc, ethis, f, ve.hasOverloads);
                        e = e.expressionSemantic(sc);
                        result = e;
                        return;
                    }
                    if (sc.func && !sc.intypeof && !(sc.flags & SCOPE.debug_))
                    {
                        sc.setUnsafe(false, exp.loc,
                            "`this` reference necessary to take address of member `%s` in `@safe` function `%s`",
                            f, sc.func);
                    }
                }
            }
        }
        else if (exp.e1.op == EXP.index)
        {
            /* For:
             *   int[3] a;
             *   &a[i]
             * check 'a' the same as for a regular variable
             */
            if (VarDeclaration v = expToVariable(exp.e1))
            {
                exp.e1.checkPurity(sc, v);
            }
        }
        else if (wasCond)
        {
            /* a ? b : c was transformed to *(a ? &b : &c), but we still
             * need to do safety checks
             */
            assert(exp.e1.op == EXP.star);
            PtrExp pe = cast(PtrExp)exp.e1;
            assert(pe.e1.op == EXP.question);
            CondExp ce = cast(CondExp)pe.e1;
            assert(ce.e1.op == EXP.address);
            assert(ce.e2.op == EXP.address);

            // Re-run semantic on the address expressions only
            ce.e1.type = null;
            ce.e1 = ce.e1.expressionSemantic(sc);
            ce.e2.type = null;
            ce.e2 = ce.e2.expressionSemantic(sc);
        }
        result = exp.optimize(WANTvalue);
    }

    override void visit(PtrExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("PtrExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        exp.e1 = exp.e1.arrayFuncConv(sc);

        Type tb = exp.e1.type.toBasetype();
        switch (tb.ty)
        {
        case Tpointer:
            exp.type = (cast(TypePointer)tb).next;
            break;

        case Tsarray:
        case Tarray:
            if (isNonAssignmentArrayOp(exp.e1))
                goto default;
            exp.error("using `*` on an array is no longer supported; use `*(%s).ptr` instead", exp.e1.toChars());
            exp.type = (cast(TypeArray)tb).next;
            exp.e1 = exp.e1.castTo(sc, exp.type.pointerTo());
            break;

        case Terror:
            return setError();

        case Tnull:
            exp.type = Type.tnoreturn;  // typeof(*null) is bottom type
            break;

        default:
            exp.error("can only `*` a pointer, not a `%s`", exp.e1.type.toChars());
            goto case Terror;
        }

        if (sc.flags & SCOPE.Cfile && exp.type && exp.type.toBasetype().ty == Tvoid)
        {
            // https://issues.dlang.org/show_bug.cgi?id=23752
            // `&*((void*)(0))` is allowed in C
            result = exp;
            return;
        }

        if (exp.checkValue())
            return setError();

        result = exp;
    }

    override void visit(NegExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("NegExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        fix16997(sc, exp);
        exp.type = exp.e1.type;
        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp.e1))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }
        if (!target.isVectorOpSupported(tb, exp.op))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if (exp.e1.checkNoBool())
            return setError();
        if (exp.e1.checkArithmetic() ||
            exp.e1.checkSharedAccess(sc))
            return setError();

        result = exp;
    }

    override void visit(UAddExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("UAddExp::semantic('%s')\n", exp.toChars());
        }
        assert(!exp.type);

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        fix16997(sc, exp);
        if (!target.isVectorOpSupported(exp.e1.type.toBasetype(), exp.op))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if (exp.e1.checkNoBool())
            return setError();
        if (exp.e1.checkArithmetic())
            return setError();
        if (exp.e1.checkSharedAccess(sc))
            return setError();

        result = exp.e1;
    }

    override void visit(ComExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        fix16997(sc, exp);
        exp.type = exp.e1.type;
        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp.e1))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }
        if (!target.isVectorOpSupported(tb, exp.op))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if (exp.e1.checkNoBool())
            return setError();
        if (exp.e1.checkIntegral() ||
            exp.e1.checkSharedAccess(sc))
            return setError();

        result = exp;
    }

    override void visit(NotExp e)
    {
        if (e.type)
        {
            result = e;
            return;
        }

        e.setNoderefOperand();

        // Note there is no operator overload
        if (Expression ex = unaSemantic(e, sc))
        {
            result = ex;
            return;
        }

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (e.e1.op == EXP.type)
            e.e1 = resolveAliasThis(sc, e.e1);

        e.e1 = resolveProperties(sc, e.e1);
        e.e1 = e.e1.toBoolean(sc);
        if (e.e1.type == Type.terror)
        {
            result = e.e1;
            return;
        }

        if (!target.isVectorOpSupported(e.e1.type.toBasetype(), e.op))
        {
            result = e.incompatibleTypes();
        }
        // https://issues.dlang.org/show_bug.cgi?id=13910
        // Today NotExp can take an array as its operand.
        if (checkNonAssignmentArrayOp(e.e1))
            return setError();

        e.type = (sc && sc.flags & SCOPE.Cfile) ? Type.tint32 : Type.tbool;
        result = e;
    }

    override void visit(DeleteExp exp)
    {
        // @@@DEPRECATED_2.109@@@
        // 1. Deprecated since 2.079
        // 2. Error since 2.099
        // 3. Removal of keyword, "delete" can be used for other identities
        if (!exp.isRAII)
        {
            error(exp.loc, "the `delete` keyword is obsolete");
            errorSupplemental(exp.loc, "use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead");
            return setError();
        }

        Expression e = exp;

        if (Expression ex = unaSemantic(exp, sc))
        {
            result = ex;
            return;
        }
        exp.e1 = resolveProperties(sc, exp.e1);
        exp.e1 = exp.e1.modifiableLvalue(sc, null);
        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        exp.type = Type.tvoid;

        Type tb = exp.e1.type.toBasetype();

        /* Now that `delete` in user code is an error, we only get here when
         * `isRAII` has been set to true for the deletion of a `scope class`.  */
        if (tb.ty != Tclass)
        {
            exp.error("cannot delete type `%s`", exp.e1.type.toChars());
            return setError();
        }

        ClassDeclaration cd = (cast(TypeClass)tb).sym;
        if (cd.isCOMinterface())
        {
            /* Because COM classes are deleted by IUnknown.Release()
             */
            exp.error("cannot `delete` instance of COM interface `%s`", cd.toChars());
            return setError();
        }

        bool err = false;
        if (cd.dtor)
        {
            err |= !cd.dtor.functionSemantic();
            err |= exp.checkPurity(sc, cd.dtor);
            err |= exp.checkSafety(sc, cd.dtor);
            err |= exp.checkNogc(sc, cd.dtor);
        }
        if (err)
            return setError();

        result = e;
    }

    override void visit(CastExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("CastExp::semantic('%s')\n", exp.toChars());
        }
        //static int x; assert(++x < 10);
        if (exp.type)
        {
            result = exp;
            return;
        }

        if ((sc && sc.flags & SCOPE.Cfile) &&
            exp.to && (exp.to.ty == Tident || exp.to.ty == Tsarray) &&
            (exp.e1.op == EXP.address || exp.e1.op == EXP.star ||
             exp.e1.op == EXP.uadd || exp.e1.op == EXP.negate))
        {
            /* Ambiguous cases arise from CParser if type-name is just an identifier.
             *   ( identifier ) cast-expression
             *   ( identifier [expression]) cast-expression
             * If we determine that `identifier` is a variable, and cast-expression
             * is one of the unary operators (& * + -), then rewrite this cast
             * as a binary expression.
             */
            Loc loc = exp.loc;
            Type t;
            Expression e;
            Dsymbol s;
            exp.to.resolve(loc, sc, e, t, s);
            if (e !is null)
            {
                if (auto ex = exp.e1.isAddrExp())       // (ident) &exp -> (ident & exp)
                    result = new AndExp(loc, e, ex.e1);
                else if (auto ex = exp.e1.isPtrExp())   // (ident) *exp -> (ident * exp)
                    result = new MulExp(loc, e, ex.e1);
                else if (auto ex = exp.e1.isUAddExp())  // (ident) +exp -> (ident + exp)
                    result = new AddExp(loc, e, ex.e1);
                else if (auto ex = exp.e1.isNegExp())   // (ident) -exp -> (ident - exp)
                    result = new MinExp(loc, e, ex.e1);

                assert(result);
                result = result.expressionSemantic(sc);
                return;
            }
        }

        if (exp.to)
        {
            exp.to = exp.to.typeSemantic(exp.loc, sc);
            if (exp.to == Type.terror)
                return setError();

            if (!exp.to.hasPointers())
                exp.setNoderefOperand();

            // When e1 is a template lambda, this cast may instantiate it with
            // the type 'to'.
            exp.e1 = inferType(exp.e1, exp.to);
        }

        if (auto e = unaSemantic(exp, sc))
        {
            result = e;
            return;
        }

        if (exp.to && !exp.to.isTypeSArray() && !exp.to.isTypeFunction())
            exp.e1 = exp.e1.arrayFuncConv(sc);

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (exp.e1.op == EXP.type)
            exp.e1 = resolveAliasThis(sc, exp.e1);

        auto e1x = resolveProperties(sc, exp.e1);
        if (e1x.op == EXP.error)
        {
            result = e1x;
            return;
        }
        if (e1x.checkType())
            return setError();
        exp.e1 = e1x;

        if (!exp.e1.type)
        {
            exp.error("cannot cast `%s`", exp.e1.toChars());
            return setError();
        }

        // https://issues.dlang.org/show_bug.cgi?id=19954
        if (exp.e1.type.ty == Ttuple)
        {
            if (exp.to)
            {
                if (TypeTuple tt = exp.to.isTypeTuple())
                {
                    if (exp.e1.type.implicitConvTo(tt))
                    {
                        result = exp.e1.castTo(sc, tt);
                        return;
                    }
                }
            }
            TupleExp te = exp.e1.isTupleExp();
            if (te.exps.length == 1)
                exp.e1 = (*te.exps)[0];
        }

        // only allow S(x) rewrite if cast specified S explicitly.
        // See https://issues.dlang.org/show_bug.cgi?id=18545
        const bool allowImplicitConstruction = exp.to !is null;

        if (!exp.to) // Handle cast(const) and cast(immutable), etc.
        {
            exp.to = exp.e1.type.castMod(exp.mod);
            exp.to = exp.to.typeSemantic(exp.loc, sc);

            if (exp.to == Type.terror)
                return setError();
        }

        if (exp.to.ty == Ttuple)
        {
            exp.error("cannot cast `%s` of type `%s` to tuple type `%s`", exp.e1.toChars(), exp.e1.type.toChars(), exp.to.toChars());
            return setError();
        }

        // cast(void) is used to mark e1 as unused, so it is safe
        if (exp.to.ty == Tvoid)
        {
            exp.type = exp.to;
            result = exp;
            return;
        }

        if (!exp.to.equals(exp.e1.type) && exp.mod == cast(ubyte)~0)
        {
            if (Expression e = exp.op_overload(sc))
            {
                result = e.implicitCastTo(sc, exp.to);
                return;
            }
        }

        Type t1b = exp.e1.type.toBasetype();
        Type tob = exp.to.toBasetype();

        if (allowImplicitConstruction && tob.ty == Tstruct && !tob.equals(t1b))
        {
            /* Look to replace:
             *  cast(S)t
             * with:
             *  S(t)
             */

            // Rewrite as to.call(e1)
            Expression e = new TypeExp(exp.loc, exp.to);
            e = new CallExp(exp.loc, e, exp.e1);
            e = e.trySemantic(sc);
            if (e)
            {
                result = e;
                return;
            }
        }

        if (!t1b.equals(tob) && (t1b.ty == Tarray || t1b.ty == Tsarray))
        {
            if (checkNonAssignmentArrayOp(exp.e1))
                return setError();
        }

        // Look for casting to a vector type
        if (tob.ty == Tvector && t1b.ty != Tvector)
        {
            result = new VectorExp(exp.loc, exp.e1, exp.to);
            result = result.expressionSemantic(sc);
            return;
        }

        Expression ex = exp.e1.castTo(sc, exp.to);
        if (ex.op == EXP.error)
        {
            result = ex;
            return;
        }

        // Check for unsafe casts
        if (!isSafeCast(ex, t1b, tob))
        {
            if (sc.setUnsafe(false, exp.loc, "cast from `%s` to `%s` not allowed in safe code", exp.e1.type, exp.to))
            {
                return setError();
            }
        }

        // `object.__ArrayCast` is a rewrite of an old runtime hook `_d_arraycast`. `_d_arraycast` was not built
        // to handle certain casts.  Those casts which `object.__ArrayCast` does not support are filtered out.
        // See `e2ir.toElemCast` for other types of casts.  If `object.__ArrayCast` is improved to support more
        // casts these conditions and potentially some logic in `e2ir.toElemCast` can be removed.
        if (tob.ty == Tarray)
        {
            // https://issues.dlang.org/show_bug.cgi?id=19840
            if (auto ad = isAggregate(t1b))
            {
                if (ad.aliasthis)
                {
                    Expression e = resolveAliasThis(sc, exp.e1);
                    e = new CastExp(exp.loc, e, exp.to);
                    result = e.expressionSemantic(sc);
                    return;
                }
            }

            if(t1b.ty == Tarray && exp.e1.op != EXP.arrayLiteral && (sc.flags & SCOPE.ctfe) == 0)
            {
                auto tFrom = t1b.nextOf();
                auto tTo = tob.nextOf();

                // https://issues.dlang.org/show_bug.cgi?id=20130
                if (exp.e1.op != EXP.string_ || !ex.isStringExp)
                {
                    const uint fromSize = cast(uint)tFrom.size();
                    const uint toSize = cast(uint)tTo.size();
                    if (fromSize == SIZE_INVALID || toSize == SIZE_INVALID)
                        return setError();

                    // If array element sizes do not match, we must adjust the dimensions
                    if (fromSize != toSize)
                    {
                        if (!verifyHookExist(exp.loc, *sc, Id.__ArrayCast, "casting array of structs"))
                            return setError();

                        // A runtime check is needed in case arrays don't line up.  That check should
                        // be done in the implementation of `object.__ArrayCast`
                        if (toSize == 0 || (fromSize % toSize) != 0)
                        {
                            // lower to `object.__ArrayCast!(TFrom, TTo)(from)`

                            // fully qualify as `object.__ArrayCast`
                            Expression id = new IdentifierExp(exp.loc, Id.empty);
                            auto dotid = new DotIdExp(exp.loc, id, Id.object);

                            auto tiargs = new Objects();
                            tiargs.push(tFrom);
                            tiargs.push(tTo);
                            auto dt = new DotTemplateInstanceExp(exp.loc, dotid, Id.__ArrayCast, tiargs);

                            auto arguments = new Expressions();
                            arguments.push(exp.e1);
                            Expression ce = new CallExp(exp.loc, dt, arguments);

                            result = expressionSemantic(ce, sc);
                            return;
                        }
                    }
                }
            }
        }

        if (sc && sc.flags & SCOPE.Cfile)
        {
            /* C11 6.5.4-5: A cast does not yield an lvalue.
             * So ensure that castTo does not strip away the cast so that this
             * can be enforced in other semantic visitor methods.
             */
            if (!ex.isCastExp())
            {
                ex = new CastExp(exp.loc, ex, exp.to);
                ex.type = exp.to;
            }
        }
        result = ex;
    }

    override void visit(VectorExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("VectorExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        exp.e1 = exp.e1.expressionSemantic(sc);
        exp.type = exp.to.typeSemantic(exp.loc, sc);
        if (exp.e1.op == EXP.error || exp.type.ty == Terror)
        {
            result = exp.e1;
            return;
        }

        Type tb = exp.type.toBasetype();
        assert(tb.ty == Tvector);
        TypeVector tv = cast(TypeVector)tb;
        Type te = tv.elementType();
        exp.dim = cast(int)(tv.size(exp.loc) / te.size(exp.loc));

        bool checkElem(Expression elem)
        {
            if (elem.isConst() == 1)
                return false;

             exp.error("constant expression expected, not `%s`", elem.toChars());
             return true;
        }

        exp.e1 = exp.e1.optimize(WANTvalue);
        bool res;
        if (exp.e1.op == EXP.arrayLiteral)
        {
            foreach (i; 0 .. exp.dim)
            {
                // Do not stop on first error - check all AST nodes even if error found
                res |= checkElem(exp.e1.isArrayLiteralExp()[i]);
            }
        }
        else if (exp.e1.type.ty == Tvoid)
            checkElem(exp.e1);

        result = res ? ErrorExp.get() : exp;
    }

    override void visit(VectorArrayExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("VectorArrayExp::semantic('%s')\n", e.toChars());
        }
        if (!e.type)
        {
            unaSemantic(e, sc);
            e.e1 = resolveProperties(sc, e.e1);

            if (e.e1.op == EXP.error)
            {
                result = e.e1;
                return;
            }
            assert(e.e1.type.ty == Tvector);
            e.type = e.e1.type.isTypeVector().basetype;
        }
        result = e;
    }

    override void visit(SliceExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("SliceExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        // operator overloading should be handled in ArrayExp already.
        if (Expression ex = unaSemantic(exp, sc))
        {
            result = ex;
            return;
        }
        exp.e1 = resolveProperties(sc, exp.e1);
        if (exp.e1.op == EXP.type && exp.e1.type.ty != Ttuple)
        {
            if (exp.lwr || exp.upr)
            {
                exp.error("cannot slice type `%s`", exp.e1.toChars());
                return setError();
            }
            Expression e = new TypeExp(exp.loc, exp.e1.type.arrayOf());
            result = e.expressionSemantic(sc);
            return;
        }
        if (!exp.lwr && !exp.upr)
        {
            if (exp.e1.op == EXP.arrayLiteral)
            {
                // Convert [a,b,c][] to [a,b,c]
                Type t1b = exp.e1.type.toBasetype();
                Expression e = exp.e1;
                if (t1b.ty == Tsarray)
                {
                    e = e.copy();
                    e.type = t1b.nextOf().arrayOf();
                }
                result = e;
                return;
            }
            if (exp.e1.op == EXP.slice)
            {
                // Convert e[][] to e[]
                SliceExp se = cast(SliceExp)exp.e1;
                if (!se.lwr && !se.upr)
                {
                    result = se;
                    return;
                }
            }
            if (isArrayOpOperand(exp.e1))
            {
                // Convert (a[]+b[])[] to a[]+b[]
                result = exp.e1;
                return;
            }
        }
        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        if (exp.e1.type.ty == Terror)
            return setError();

        Type t1b = exp.e1.type.toBasetype();
        if (auto tp = t1b.isTypePointer())
        {
            if (t1b.isPtrToFunction())
            {
                exp.error("cannot slice function pointer `%s`", exp.e1.toChars());
                return setError();
            }
            if (!exp.lwr || !exp.upr)
            {
                exp.error("upper and lower bounds are needed to slice a pointer");
                if (auto ad = isAggregate(tp.next.toBasetype()))
                {
                    auto s = search_function(ad, Id.index);
                    if (!s) s = search_function(ad, Id.slice);
                    if (s)
                    {
                        auto fd = s.isFuncDeclaration();
                        if ((fd && !fd.getParameterList().length) || s.isTemplateDeclaration())
                        {
                            exp.errorSupplemental(
                                "pointer `%s` points to an aggregate that defines an `%s`, perhaps you meant `(*%s)[]`",
                                exp.e1.toChars(),
                                s.ident.toChars(),
                                exp.e1.toChars()
                            );
                        }

                    }
                }

                return setError();
            }
            if (sc.setUnsafe(false, exp.loc, "pointer slicing not allowed in safe functions"))
                return setError();
        }
        else if (t1b.ty == Tarray)
        {
        }
        else if (t1b.ty == Tsarray)
        {
        }
        else if (t1b.ty == Ttuple)
        {
            if (!exp.lwr && !exp.upr)
            {
                result = exp.e1;
                return;
            }
            if (!exp.lwr || !exp.upr)
            {
                exp.error("need upper and lower bound to slice tuple");
                return setError();
            }
        }
        else if (t1b.ty == Tvector && exp.e1.isLvalue())
        {
            // Convert e1 to corresponding static array
            TypeVector tv1 = cast(TypeVector)t1b;
            t1b = tv1.basetype;
            t1b = t1b.castMod(tv1.mod);
            exp.e1.type = t1b;
        }
        else
        {
            exp.error("`%s` cannot be sliced with `[]`", t1b.ty == Tvoid ? exp.e1.toChars() : t1b.toChars());
            return setError();
        }

        /* Run semantic on lwr and upr.
         */
        Scope* scx = sc;
        if (t1b.ty == Tsarray || t1b.ty == Tarray || t1b.ty == Ttuple)
        {
            // Create scope for 'length' variable
            ScopeDsymbol sym = new ArrayScopeSymbol(sc, exp);
            sym.parent = sc.scopesym;
            sc = sc.push(sym);
        }
        if (exp.lwr)
        {
            if (t1b.ty == Ttuple)
                sc = sc.startCTFE();
            exp.lwr = exp.lwr.expressionSemantic(sc);
            exp.lwr = resolveProperties(sc, exp.lwr);
            if (t1b.ty == Ttuple)
                sc = sc.endCTFE();
            exp.lwr = exp.lwr.implicitCastTo(sc, Type.tsize_t);
        }
        if (exp.upr)
        {
            if (t1b.ty == Ttuple)
                sc = sc.startCTFE();
            exp.upr = exp.upr.expressionSemantic(sc);
            exp.upr = resolveProperties(sc, exp.upr);
            if (t1b.ty == Ttuple)
                sc = sc.endCTFE();
            exp.upr = exp.upr.implicitCastTo(sc, Type.tsize_t);
        }
        if (sc != scx)
            sc = sc.pop();
        if (exp.lwr && exp.lwr.type == Type.terror || exp.upr && exp.upr.type == Type.terror)
            return setError();

        if (t1b.ty == Ttuple)
        {
            exp.lwr = exp.lwr.ctfeInterpret();
            exp.upr = exp.upr.ctfeInterpret();
            uinteger_t i1 = exp.lwr.toUInteger();
            uinteger_t i2 = exp.upr.toUInteger();

            TupleExp te;
            TypeTuple tup;
            size_t length;
            if (exp.e1.op == EXP.tuple) // slicing an expression tuple
            {
                te = cast(TupleExp)exp.e1;
                tup = null;
                length = te.exps.length;
            }
            else if (exp.e1.op == EXP.type) // slicing a type tuple
            {
                te = null;
                tup = cast(TypeTuple)t1b;
                length = Parameter.dim(tup.arguments);
            }
            else
                assert(0);

            if (i2 < i1 || length < i2)
            {
                exp.error("string slice `[%llu .. %llu]` is out of bounds", i1, i2);
                return setError();
            }

            size_t j1 = cast(size_t)i1;
            size_t j2 = cast(size_t)i2;
            Expression e;
            if (exp.e1.op == EXP.tuple)
            {
                auto exps = new Expressions(j2 - j1);
                for (size_t i = 0; i < j2 - j1; i++)
                {
                    (*exps)[i] = (*te.exps)[j1 + i];
                }
                e = new TupleExp(exp.loc, te.e0, exps);
            }
            else
            {
                auto args = new Parameters();
                args.reserve(j2 - j1);
                for (size_t i = j1; i < j2; i++)
                {
                    Parameter arg = Parameter.getNth(tup.arguments, i);
                    args.push(arg);
                }
                e = new TypeExp(exp.e1.loc, new TypeTuple(args));
            }
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }

        exp.type = t1b.nextOf().arrayOf();
        // Allow typedef[] -> typedef[]
        if (exp.type.equals(t1b))
            exp.type = exp.e1.type;

        // We might know $ now
        setLengthVarIfKnown(exp.lengthVar, t1b);

        if (exp.lwr && exp.upr)
        {
            exp.lwr = exp.lwr.optimize(WANTvalue);
            exp.upr = exp.upr.optimize(WANTvalue);

            IntRange lwrRange = getIntRange(exp.lwr);
            IntRange uprRange = getIntRange(exp.upr);

            if (t1b.ty == Tsarray || t1b.ty == Tarray)
            {
                Expression el = new ArrayLengthExp(exp.loc, exp.e1);
                el = el.expressionSemantic(sc);
                el = el.optimize(WANTvalue);
                if (el.op == EXP.int64)
                {
                    // Array length is known at compile-time. Upper is in bounds if it fits length.
                    dinteger_t length = el.toInteger();
                    auto bounds = IntRange(SignExtendedNumber(0), SignExtendedNumber(length));
                    exp.upperIsInBounds = bounds.contains(uprRange);
                }
                else if (exp.upr.op == EXP.int64 && exp.upr.toInteger() == 0)
                {
                    // Upper slice expression is '0'. Value is always in bounds.
                    exp.upperIsInBounds = true;
                }
                else if (exp.upr.op == EXP.variable && (cast(VarExp)exp.upr).var.ident == Id.dollar)
                {
                    // Upper slice expression is '$'. Value is always in bounds.
                    exp.upperIsInBounds = true;
                }
            }
            else if (t1b.ty == Tpointer)
            {
                exp.upperIsInBounds = true;
            }
            else
                assert(0);

            exp.lowerIsLessThanUpper = (lwrRange.imax <= uprRange.imin);

            //printf("upperIsInBounds = %d lowerIsLessThanUpper = %d\n", exp.upperIsInBounds, exp.lowerIsLessThanUpper);
        }

        result = exp;
    }

    override void visit(ArrayLengthExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("ArrayLengthExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        if (Expression ex = unaSemantic(e, sc))
        {
            result = ex;
            return;
        }
        e.e1 = resolveProperties(sc, e.e1);

        e.type = Type.tsize_t;
        result = e;
    }

    override void visit(ArrayExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("ArrayExp::semantic('%s')\n", exp.toChars());
        }
        assert(!exp.type);

        if (sc.flags & SCOPE.Cfile)
        {
            /* See if need to rewrite the AST because of cast/call ambiguity
             */
            if (auto e = castCallAmbiguity(exp, sc))
            {
                result = expressionSemantic(e, sc);
                return;
            }
        }

        result = exp.carraySemantic(sc);  // C semantics
        if (result)
            return;

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (isAggregate(exp.e1.type))
            exp.error("no `[]` operator overload for type `%s`", exp.e1.type.toChars());
        else if (exp.e1.op == EXP.type && exp.e1.type.ty != Ttuple)
            exp.error("static array of `%s` with multiple lengths not allowed", exp.e1.type.toChars());
        else if (isIndexableNonAggregate(exp.e1.type))
            exp.error("only one index allowed to index `%s`", exp.e1.type.toChars());
        else
            exp.error("cannot use `[]` operator on expression of type `%s`", exp.e1.type.toChars());

        result = ErrorExp.get();
    }

    override void visit(DotExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotExp::semantic('%s')\n", exp.toChars());
            if (exp.type)
                printf("\ttype = %s\n", exp.type.toChars());
        }
        exp.e1 = exp.e1.expressionSemantic(sc);
        exp.e2 = exp.e2.expressionSemantic(sc);

        if (exp.e1.op == EXP.type)
        {
            result = exp.e2;
            return;
        }
        if (exp.e2.op == EXP.type)
        {
            result = exp.e2;
            return;
        }
        if (auto te = exp.e2.isTemplateExp())
        {
            Expression e = new DotTemplateExp(exp.loc, exp.e1, te.td);
            result = e.expressionSemantic(sc);
            return;
        }
        if (!exp.type)
            exp.type = exp.e2.type;
        result = exp;
    }

    override void visit(CommaExp e)
    {
        //printf("Semantic.CommaExp() %s\n", e.toChars());
        if (e.type)
        {
            result = e;
            return;
        }

        // Allow `((a,b),(x,y))`
        if (e.allowCommaExp)
        {
            CommaExp.allow(e.e1);
            CommaExp.allow(e.e2);
        }

        if (Expression ex = binSemanticProp(e, sc))
        {
            result = ex;
            return;
        }
        e.e1 = e.e1.addDtorHook(sc);

        if (checkNonAssignmentArrayOp(e.e1))
            return setError();

        // Comma expressions trigger this conversion
        e.e2 = e.e2.arrayFuncConv(sc);

        e.type = e.e2.type;
        result = e;

        if (sc.flags & SCOPE.Cfile)
            return;

        if (e.type is Type.tvoid)
        {
            checkMustUse(e.e1, sc);
            discardValue(e.e1);
        }
        else if (!e.allowCommaExp && !e.isGenerated)
            e.error("using the result of a comma expression is not allowed");
    }

    override void visit(IntervalExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("IntervalExp::semantic('%s')\n", e.toChars());
        }
        if (e.type)
        {
            result = e;
            return;
        }

        Expression le = e.lwr;
        le = le.expressionSemantic(sc);
        le = resolveProperties(sc, le);

        Expression ue = e.upr;
        ue = ue.expressionSemantic(sc);
        ue = resolveProperties(sc, ue);

        if (le.op == EXP.error)
        {
            result = le;
            return;
        }
        if (ue.op == EXP.error)
        {
            result = ue;
            return;
        }

        e.lwr = le;
        e.upr = ue;

        e.type = Type.tvoid;
        result = e;
    }

    override void visit(DelegatePtrExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("DelegatePtrExp::semantic('%s')\n", e.toChars());
        }
        if (!e.type)
        {
            unaSemantic(e, sc);
            e.e1 = resolveProperties(sc, e.e1);

            if (e.e1.op == EXP.error)
            {
                result = e.e1;
                return;
            }
            e.type = Type.tvoidptr;
        }
        result = e;
    }

    override void visit(DelegateFuncptrExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("DelegateFuncptrExp::semantic('%s')\n", e.toChars());
        }
        if (!e.type)
        {
            unaSemantic(e, sc);
            e.e1 = resolveProperties(sc, e.e1);
            if (e.e1.op == EXP.error)
            {
                result = e.e1;
                return;
            }
            e.type = e.e1.type.nextOf().pointerTo();
        }
        result = e;
    }

    override void visit(IndexExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("IndexExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        // operator overloading should be handled in ArrayExp already.
        if (!exp.e1.type)
            exp.e1 = exp.e1.expressionSemantic(sc).arrayFuncConv(sc);
        assert(exp.e1.type); // semantic() should already be run on it
        if (exp.e1.op == EXP.type && exp.e1.type.ty != Ttuple)
        {
            exp.e2 = exp.e2.expressionSemantic(sc);
            exp.e2 = resolveProperties(sc, exp.e2);
            Type nt;
            if (exp.e2.op == EXP.type)
                nt = new TypeAArray(exp.e1.type, exp.e2.type);
            else
                nt = new TypeSArray(exp.e1.type, exp.e2);
            Expression e = new TypeExp(exp.loc, nt);
            result = e.expressionSemantic(sc);
            return;
        }
        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        if (exp.e1.type.ty == Terror)
            return setError();

        // Note that unlike C we do not implement the int[ptr]

        Type t1b = exp.e1.type.toBasetype();

        if (TypeVector tv1 = t1b.isTypeVector())
        {
            // Convert e1 to corresponding static array
            t1b = tv1.basetype;
            t1b = t1b.castMod(tv1.mod);
            exp.e1 = exp.e1.castTo(sc, t1b);
        }
        if (t1b.ty == Tsarray || t1b.ty == Tarray)
        {
            if (!checkAddressable(exp, sc))
                return setError();
        }

        /* Run semantic on e2
         */
        Scope* scx = sc;
        if (t1b.ty == Tsarray || t1b.ty == Tarray || t1b.ty == Ttuple)
        {
            // Create scope for 'length' variable
            ScopeDsymbol sym = new ArrayScopeSymbol(sc, exp);
            sym.parent = sc.scopesym;
            sc = sc.push(sym);
        }
        if (t1b.ty == Ttuple)
            sc = sc.startCTFE();
        exp.e2 = exp.e2.expressionSemantic(sc).arrayFuncConv(sc);
        exp.e2 = resolveProperties(sc, exp.e2);
        if (t1b.ty == Ttuple)
            sc = sc.endCTFE();
        if (exp.e2.op == EXP.tuple)
        {
            TupleExp te = cast(TupleExp)exp.e2;
            if (te.exps && te.exps.length == 1)
                exp.e2 = Expression.combine(te.e0, (*te.exps)[0]); // bug 4444 fix
        }
        if (sc != scx)
            sc = sc.pop();
        if (exp.e2.type == Type.terror)
            return setError();

        if (checkNonAssignmentArrayOp(exp.e1))
            return setError();

        switch (t1b.ty)
        {
        case Tpointer:
            if (t1b.isPtrToFunction())
            {
                exp.error("cannot index function pointer `%s`", exp.e1.toChars());
                return setError();
            }
            exp.e2 = exp.e2.implicitCastTo(sc, Type.tsize_t);
            if (exp.e2.type == Type.terror)
                return setError();
            exp.e2 = exp.e2.optimize(WANTvalue);
            if (exp.e2.op == EXP.int64 && exp.e2.toInteger() == 0)
            {
            }
            else if (sc.setUnsafe(false, exp.loc, "`@safe` function `%s` cannot index pointer `%s`", sc.func, exp.e1))
            {
                return setError();
            }
            exp.type = (cast(TypeNext)t1b).next;
            break;

        case Tarray:
            exp.e2 = exp.e2.implicitCastTo(sc, Type.tsize_t);
            if (exp.e2.type == Type.terror)
                return setError();
            exp.type = (cast(TypeNext)t1b).next;
            break;

        case Tsarray:
            {
                exp.e2 = exp.e2.implicitCastTo(sc, Type.tsize_t);
                if (exp.e2.type == Type.terror)
                    return setError();
                exp.type = t1b.nextOf();
                break;
            }
        case Taarray:
            {
                TypeAArray taa = cast(TypeAArray)t1b;
                /* We can skip the implicit conversion if they differ only by
                 * constness
                 * https://issues.dlang.org/show_bug.cgi?id=2684
                 * see also bug https://issues.dlang.org/show_bug.cgi?id=2954 b
                 */
                if (!arrayTypeCompatibleWithoutCasting(exp.e2.type, taa.index))
                {
                    exp.e2 = exp.e2.implicitCastTo(sc, taa.index); // type checking
                    if (exp.e2.type == Type.terror)
                        return setError();
                }

                semanticTypeInfo(sc, taa);
                checkNewEscape(sc, exp.e2, false);

                exp.type = taa.next;
                break;
            }
        case Ttuple:
            {
                exp.e2 = exp.e2.implicitCastTo(sc, Type.tsize_t);
                if (exp.e2.type == Type.terror)
                    return setError();

                exp.e2 = exp.e2.ctfeInterpret();
                uinteger_t index = exp.e2.toUInteger();

                TupleExp te;
                TypeTuple tup;
                size_t length;
                if (exp.e1.op == EXP.tuple)
                {
                    te = cast(TupleExp)exp.e1;
                    tup = null;
                    length = te.exps.length;
                }
                else if (exp.e1.op == EXP.type)
                {
                    te = null;
                    tup = cast(TypeTuple)t1b;
                    length = Parameter.dim(tup.arguments);
                }
                else
                    assert(0);

                if (length <= index)
                {
                    exp.error("array index `[%llu]` is outside array bounds `[0 .. %llu]`", index, cast(ulong)length);
                    return setError();
                }
                Expression e;
                if (exp.e1.op == EXP.tuple)
                {
                    e = (*te.exps)[cast(size_t)index];
                    e = Expression.combine(te.e0, e);
                }
                else
                    e = new TypeExp(exp.e1.loc, Parameter.getNth(tup.arguments, cast(size_t)index).type);
                result = e;
                return;
            }
        default:
            exp.error("`%s` must be an array or pointer type, not `%s`", exp.e1.toChars(), exp.e1.type.toChars());
            return setError();
        }

        // We might know $ now
        setLengthVarIfKnown(exp.lengthVar, t1b);

        if (t1b.ty == Tsarray || t1b.ty == Tarray)
        {
            Expression el = new ArrayLengthExp(exp.loc, exp.e1);
            el = el.expressionSemantic(sc);
            el = el.optimize(WANTvalue);
            if (el.op == EXP.int64)
            {
                exp.e2 = exp.e2.optimize(WANTvalue);
                dinteger_t length = el.toInteger();
                if (length)
                {
                    auto bounds = IntRange(SignExtendedNumber(0), SignExtendedNumber(length - 1));
                    // OR it in, because it might already be set for C array indexing
                    exp.indexIsInBounds |= bounds.contains(getIntRange(exp.e2));
                }
                else if (sc.flags & SCOPE.Cfile && t1b.ty == Tsarray)
                {
                    if (auto ve = exp.e1.isVarExp())
                    {
                        /* Rewrite 0-length C array ve[exp.e2] as *(ve + exp.e2)
                         */
                        auto vp = ve.castTo(sc, t1b.isTypeSArray().next.pointerTo());
                        auto e = new AddExp(exp.loc, vp, exp.e2);
                        auto pe = new PtrExp(exp.loc, e);
                        result = pe.expressionSemantic(sc).optimize(WANTvalue);
                        return;
                    }
                }
            }
        }

        result = exp;
    }

    override void visit(PostExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("PostExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (sc.flags & SCOPE.Cfile)
        {
            /* See if need to rewrite the AST because of cast/call ambiguity
             */
            if (auto e = castCallAmbiguity(exp, sc))
            {
                result = expressionSemantic(e, sc);
                return;
            }
        }

        if (Expression ex = binSemantic(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e1x = resolveProperties(sc, exp.e1);
        if (e1x.op == EXP.error)
        {
            result = e1x;
            return;
        }
        exp.e1 = e1x;

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.e1.checkReadModifyWrite(exp.op))
            return setError();

        if (exp.e1.op == EXP.slice)
        {
            const(char)* s = exp.op == EXP.plusPlus ? "increment" : "decrement";
            exp.error("cannot post-%s array slice `%s`, use pre-%s instead", s, exp.e1.toChars(), s);
            return setError();
        }

        Type t1 = exp.e1.type.toBasetype();
        if (t1.ty == Tclass || t1.ty == Tstruct || exp.e1.op == EXP.arrayLength)
        {
            /* Check for operator overloading,
             * but rewrite in terms of ++e instead of e++
             */

            /* If e1 is not trivial, take a reference to it
             */
            Expression de = null;
            if (exp.e1.op != EXP.variable && exp.e1.op != EXP.arrayLength)
            {
                // ref v = e1;
                auto v = copyToTemp(STC.ref_, "__postref", exp.e1);
                de = new DeclarationExp(exp.loc, v);
                exp.e1 = new VarExp(exp.e1.loc, v);
            }

            /* Rewrite as:
             * auto tmp = e1; ++e1; tmp
             */
            auto tmp = copyToTemp(0, "__pitmp", exp.e1);
            Expression ea = new DeclarationExp(exp.loc, tmp);

            Expression eb = exp.e1.syntaxCopy();
            eb = new PreExp(exp.op == EXP.plusPlus ? EXP.prePlusPlus : EXP.preMinusMinus, exp.loc, eb);

            Expression ec = new VarExp(exp.loc, tmp);

            // Combine de,ea,eb,ec
            if (de)
                ea = new CommaExp(exp.loc, de, ea);
            e = new CommaExp(exp.loc, ea, eb);
            e = new CommaExp(exp.loc, e, ec);
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }

        exp.e1 = exp.e1.modifiableLvalue(sc, exp.e1);
        exp.e1 = exp.e1.optimize(WANTvalue, /*keepLvalue*/ true);

        e = exp;
        if (exp.e1.checkScalar() ||
            exp.e1.checkSharedAccess(sc))
            return setError();
        if (exp.e1.checkNoBool())
            return setError();

        if (exp.e1.type.ty == Tpointer)
            e = scaleFactor(exp, sc);
        else
            exp.e2 = exp.e2.castTo(sc, exp.e1.type);
        e.type = exp.e1.type;
        result = e;
    }

    override void visit(PreExp exp)
    {
        Expression e = exp.op_overload(sc);
        // printf("PreExp::semantic('%s')\n", toChars());
        if (e)
        {
            result = e;
            return;
        }

        // Rewrite as e1+=1 or e1-=1
        if (exp.op == EXP.prePlusPlus)
            e = new AddAssignExp(exp.loc, exp.e1, IntegerExp.literal!1);
        else
            e = new MinAssignExp(exp.loc, exp.e1, IntegerExp.literal!1);
        result = e.expressionSemantic(sc);
    }

    /*
     * Get the expression initializer for a specific struct
     *
     * Params:
     *  sd = the struct for which the expression initializer is needed
     *  loc = the location of the initializer
     *  sc = the scope where the expression is located
     *  t = the type of the expression
     *
     * Returns:
     *  The expression initializer or error expression if any errors occured
     */
    private Expression getInitExp(StructDeclaration sd, Loc loc, Scope* sc, Type t)
    {
        if (sd.zeroInit && !sd.isNested())
        {
            // https://issues.dlang.org/show_bug.cgi?id=14606
            // Always use BlitExp for the special expression: (struct = 0)
            return IntegerExp.literal!0;
        }

        if (sd.isNested())
        {
            auto sle = new StructLiteralExp(loc, sd, null, t);
            if (!sd.fill(loc, *sle.elements, true))
                return ErrorExp.get();
            if (checkFrameAccess(loc, sc, sd, sle.elements.length))
                return ErrorExp.get();

            sle.type = t;
            return sle;
        }

        return t.defaultInit(loc);
    }

    override void visit(AssignExp exp)
    {
        static if (LOGSEMANTIC)
        {
            if (exp.op == EXP.blit)      printf("BlitExp.toElem('%s')\n", exp.toChars());
            if (exp.op == EXP.assign)    printf("AssignExp.toElem('%s')\n", exp.toChars());
            if (exp.op == EXP.construct) printf("ConstructExp.toElem('%s')\n", exp.toChars());
        }

        void setResult(Expression e, int line = __LINE__)
        {
            //printf("line %d\n", line);
            result = e;
        }

        if (exp.type)
        {
            return setResult(exp);
        }

        Expression e1old = exp.e1;

        if (auto e2comma = exp.e2.isCommaExp())
        {
            if (!e2comma.isGenerated && !(sc.flags & SCOPE.Cfile))
                exp.error("using the result of a comma expression is not allowed");

            /* Rewrite to get rid of the comma from rvalue
             *   e1=(e0,e2) => e0,(e1=e2)
             */
            Expression e0;
            exp.e2 = Expression.extractLast(e2comma, e0);
            Expression e = Expression.combine(e0, exp);
            return setResult(e.expressionSemantic(sc));
        }

        /* Look for operator overloading of a[arguments] = e2.
         * Do it before e1.expressionSemantic() otherwise the ArrayExp will have been
         * converted to unary operator overloading already.
         */
        if (auto ae = exp.e1.isArrayExp())
        {
            Expression res;

            ae.e1 = ae.e1.expressionSemantic(sc);
            ae.e1 = resolveProperties(sc, ae.e1);
            Expression ae1old = ae.e1;

            const(bool) maybeSlice =
                (ae.arguments.length == 0 ||
                 ae.arguments.length == 1 && (*ae.arguments)[0].op == EXP.interval);

            IntervalExp ie = null;
            if (maybeSlice && ae.arguments.length)
            {
                assert((*ae.arguments)[0].op == EXP.interval);
                ie = cast(IntervalExp)(*ae.arguments)[0];
            }
            while (true)
            {
                if (ae.e1.op == EXP.error)
                    return setResult(ae.e1);

                Expression e0 = null;
                Expression ae1save = ae.e1;
                ae.lengthVar = null;

                Type t1b = ae.e1.type.toBasetype();
                AggregateDeclaration ad = isAggregate(t1b);
                if (!ad)
                    break;
                if (search_function(ad, Id.indexass))
                {
                    // Deal with $
                    res = resolveOpDollar(sc, ae, &e0);
                    if (!res) // a[i..j] = e2 might be: a.opSliceAssign(e2, i, j)
                        goto Lfallback;
                    if (res.op == EXP.error)
                        return setResult(res);

                    res = exp.e2.expressionSemantic(sc);
                    if (res.op == EXP.error)
                        return setResult(res);
                    exp.e2 = res;

                    /* Rewrite (a[arguments] = e2) as:
                     *      a.opIndexAssign(e2, arguments)
                     */
                    Expressions* a = ae.arguments.copy();
                    a.insert(0, exp.e2);
                    res = new DotIdExp(exp.loc, ae.e1, Id.indexass);
                    res = new CallExp(exp.loc, res, a);
                    if (maybeSlice) // a[] = e2 might be: a.opSliceAssign(e2)
                        res = res.trySemantic(sc);
                    else
                        res = res.expressionSemantic(sc);
                    if (res)
                        return setResult(Expression.combine(e0, res));
                }

            Lfallback:
                if (maybeSlice && search_function(ad, Id.sliceass))
                {
                    // Deal with $
                    res = resolveOpDollar(sc, ae, ie, &e0);
                    if (res.op == EXP.error)
                        return setResult(res);

                    res = exp.e2.expressionSemantic(sc);
                    if (res.op == EXP.error)
                        return setResult(res);

                    exp.e2 = res;

                    /* Rewrite (a[i..j] = e2) as:
                     *      a.opSliceAssign(e2, i, j)
                     */
                    auto a = new Expressions();
                    a.push(exp.e2);
                    if (ie)
                    {
                        a.push(ie.lwr);
                        a.push(ie.upr);
                    }
                    res = new DotIdExp(exp.loc, ae.e1, Id.sliceass);
                    res = new CallExp(exp.loc, res, a);
                    res = res.expressionSemantic(sc);
                    return setResult(Expression.combine(e0, res));
                }

                // No operator overloading member function found yet, but
                // there might be an alias this to try.
                if (ad.aliasthis && !isRecursiveAliasThis(ae.att1, ae.e1.type))
                {
                    /* Rewrite (a[arguments] op e2) as:
                     *      a.aliasthis[arguments] op e2
                     */
                    ae.e1 = resolveAliasThis(sc, ae1save, true);
                    if (ae.e1)
                        continue;
                }
                break;
            }
            ae.e1 = ae1old; // recovery
            ae.lengthVar = null;
        }

        /* Run this.e1 semantic.
         */
        {
            Expression e1x = exp.e1;

            /* With UFCS, e.f = value
             * Could mean:
             *      .f(e, value)
             * or:
             *      .f(e) = value
             */
            if (auto dti = e1x.isDotTemplateInstanceExp())
            {
                Expression e = dti.dotTemplateSemanticProp(sc, 1);
                if (!e)
                {
                    return setResult(resolveUFCSProperties(sc, e1x, exp.e2));
                }

                e1x = e;
            }
            else if (sc.flags & SCOPE.Cfile && e1x.isDotIdExp())
            {
                auto die = e1x.isDotIdExp();
                e1x = fieldLookup(die.e1, sc, die.ident);
            }
            else if (auto die = e1x.isDotIdExp())
            {
                Expression e = die.dotIdSemanticProp(sc, 1);
                if (e && isDotOpDispatch(e))
                {
                    /* https://issues.dlang.org/show_bug.cgi?id=19687
                     *
                     * On this branch, e2 is semantically analyzed in resolvePropertiesX,
                     * but that call is done with gagged errors. That is the only time when
                     * semantic gets ran on e2, that is why the error never gets to be printed.
                     * In order to make sure that UFCS is tried with correct parameters, e2
                     * needs to have semantic ran on it.
                     */
                    auto ode = e;
                    exp.e2 = exp.e2.expressionSemantic(sc);
                    uint errors = global.startGagging();
                    e = resolvePropertiesX(sc, e, exp.e2);
                    // Any error or if 'e' is not resolved, go to UFCS
                    if (global.endGagging(errors) || e is ode)
                        e = null; /* fall down to UFCS */
                    else
                        return setResult(e);
                }
                if (!e)
                    return setResult(resolveUFCSProperties(sc, e1x, exp.e2));
                e1x = e;
            }
            else
            {
                if (auto se = e1x.isSliceExp())
                    se.arrayop = true;

                e1x = e1x.expressionSemantic(sc);
            }

            /* We have f = value.
             * Could mean:
             *      f(value)
             * or:
             *      f() = value
             */
            if (Expression e = resolvePropertiesX(sc, e1x, exp.e2))
                return setResult(e);

            if (e1x.checkRightThis(sc))
            {
                return setError();
            }
            exp.e1 = e1x;
            assert(exp.e1.type);
        }
        Type t1 = exp.e1.type.isTypeEnum() ? exp.e1.type : exp.e1.type.toBasetype();

        /* Run this.e2 semantic.
         * Different from other binary expressions, the analysis of e2
         * depends on the result of e1 in assignments.
         */
        {
            Expression e2x = inferType(exp.e2, t1.baseElemOf());
            e2x = e2x.expressionSemantic(sc);
            if (!t1.isTypeSArray())
                e2x = e2x.arrayFuncConv(sc);
            e2x = resolveProperties(sc, e2x);
            if (e2x.op == EXP.type)
                e2x = resolveAliasThis(sc, e2x); //https://issues.dlang.org/show_bug.cgi?id=17684
            if (e2x.op == EXP.error)
                return setResult(e2x);
            // We delay checking the value for structs/classes as these might have
            // an opAssign defined.
            if ((t1.ty != Tstruct && t1.ty != Tclass && e2x.checkValue()) ||
                e2x.checkSharedAccess(sc))
                return setError();

            auto etmp = checkNoreturnVarAccess(e2x);
            if (etmp != e2x)
                return setResult(etmp);

            exp.e2 = e2x;
        }

        /* Rewrite tuple assignment as a tuple of assignments.
         */
        {
            Expression e2x = exp.e2;

        Ltupleassign:
            if (exp.e1.op == EXP.tuple && e2x.op == EXP.tuple)
            {
                TupleExp tup1 = cast(TupleExp)exp.e1;
                TupleExp tup2 = cast(TupleExp)e2x;
                size_t dim = tup1.exps.length;
                Expression e = null;
                if (dim != tup2.exps.length)
                {
                    exp.error("mismatched tuple lengths, %d and %d", cast(int)dim, cast(int)tup2.exps.length);
                    return setError();
                }
                if (dim == 0)
                {
                    e = IntegerExp.literal!0;
                    e = new CastExp(exp.loc, e, Type.tvoid); // avoid "has no effect" error
                    e = Expression.combine(tup1.e0, tup2.e0, e);
                }
                else
                {
                    auto exps = new Expressions(dim);
                    for (size_t i = 0; i < dim; i++)
                    {
                        Expression ex1 = (*tup1.exps)[i];
                        Expression ex2 = (*tup2.exps)[i];
                        (*exps)[i] = new AssignExp(exp.loc, ex1, ex2);
                    }
                    e = new TupleExp(exp.loc, Expression.combine(tup1.e0, tup2.e0), exps);
                }
                return setResult(e.expressionSemantic(sc));
            }

            /* Look for form: e1 = e2.aliasthis.
             */
            if (exp.e1.op == EXP.tuple)
            {
                TupleDeclaration td = isAliasThisTuple(e2x);
                if (!td)
                    goto Lnomatch;

                assert(exp.e1.type.ty == Ttuple);
                TypeTuple tt = cast(TypeTuple)exp.e1.type;

                Expression e0;
                Expression ev = extractSideEffect(sc, "__tup", e0, e2x);

                auto iexps = new Expressions();
                iexps.push(ev);
                for (size_t u = 0; u < iexps.length; u++)
                {
                Lexpand:
                    Expression e = (*iexps)[u];

                    Parameter arg = Parameter.getNth(tt.arguments, u);
                    //printf("[%d] iexps.length = %d, ", u, iexps.length);
                    //printf("e = (%s %s, %s), ", Token.toChars[e.op], e.toChars(), e.type.toChars());
                    //printf("arg = (%s, %s)\n", arg.toChars(), arg.type.toChars());

                    if (!arg || !e.type.implicitConvTo(arg.type))
                    {
                        // expand initializer to tuple
                        if (expandAliasThisTuples(iexps, u) != -1)
                        {
                            if (iexps.length <= u)
                                break;
                            goto Lexpand;
                        }
                        goto Lnomatch;
                    }
                }
                e2x = new TupleExp(e2x.loc, e0, iexps);
                e2x = e2x.expressionSemantic(sc);
                if (e2x.op == EXP.error)
                {
                    result = e2x;
                    return;
                }
                // Do not need to overwrite this.e2
                goto Ltupleassign;
            }
        Lnomatch:
        }

        /* Inside constructor, if this is the first assignment of object field,
         * rewrite this to initializing the field.
         */
        if (exp.op == EXP.assign
            && exp.e1.checkModifiable(sc) == Modifiable.initialization)
        {
            //printf("[%s] change to init - %s\n", exp.loc.toChars(), exp.toChars());
            auto t = exp.type;
            exp = new ConstructExp(exp.loc, exp.e1, exp.e2);
            exp.type = t;

            // https://issues.dlang.org/show_bug.cgi?id=13515
            // set Index::modifiable flag for complex AA element initialization
            if (auto ie1 = exp.e1.isIndexExp())
            {
                Expression e1x = ie1.markSettingAAElem();
                if (e1x.op == EXP.error)
                {
                    result = e1x;
                    return;
                }
            }
        }
        else if (exp.op == EXP.construct && exp.e1.op == EXP.variable &&
                 (cast(VarExp)exp.e1).var.storage_class & (STC.out_ | STC.ref_))
        {
            exp.memset = MemorySet.referenceInit;
        }

        if (exp.op == EXP.assign)  // skip EXP.blit and EXP.construct, which are initializations
        {
            exp.e1.checkSharedAccess(sc);
            checkUnsafeAccess(sc, exp.e1, false, true);
        }

        checkUnsafeAccess(sc, exp.e2, true, true); // Initializer must always be checked

        /* If it is an assignment from a 'foreign' type,
         * check for operator overloading.
         */
        if (exp.memset == MemorySet.referenceInit)
        {
            // If this is an initialization of a reference,
            // do nothing
        }
        else if (t1.ty == Tstruct)
        {
            auto e1x = exp.e1;
            auto e2x = exp.e2;
            auto sd = (cast(TypeStruct)t1).sym;

            if (exp.op == EXP.construct)
            {
                Type t2 = e2x.type.toBasetype();
                if (t2.ty == Tstruct && sd == (cast(TypeStruct)t2).sym)
                {
                    sd.size(exp.loc);
                    if (sd.sizeok != Sizeok.done)
                        return setError();
                    if (!sd.ctor)
                        sd.ctor = sd.searchCtor();

                    // https://issues.dlang.org/show_bug.cgi?id=15661
                    // Look for the form from last of comma chain.
                    auto e2y = lastComma(e2x);

                    CallExp ce = (e2y.op == EXP.call) ? cast(CallExp)e2y : null;
                    DotVarExp dve = (ce && ce.e1.op == EXP.dotVariable)
                        ? cast(DotVarExp)ce.e1 : null;
                    if (sd.ctor && ce && dve && dve.var.isCtorDeclaration() &&
                        // https://issues.dlang.org/show_bug.cgi?id=19389
                        dve.e1.op != EXP.dotVariable &&
                        e2y.type.implicitConvTo(t1))
                    {
                        /* Look for form of constructor call which is:
                         *    __ctmp.ctor(arguments...)
                         */

                        /* Before calling the constructor, initialize
                         * variable with a bit copy of the default
                         * initializer
                         */
                        Expression einit = getInitExp(sd, exp.loc, sc, t1);
                        if (einit.op == EXP.error)
                        {
                            result = einit;
                            return;
                        }

                        auto ae = new BlitExp(exp.loc, exp.e1, einit);
                        ae.type = e1x.type;

                        /* Replace __ctmp being constructed with e1.
                         * We need to copy constructor call expression,
                         * because it may be used in other place.
                         */
                        auto dvx = cast(DotVarExp)dve.copy();
                        dvx.e1 = e1x;
                        auto cx = cast(CallExp)ce.copy();
                        cx.e1 = dvx;
                        if (checkConstructorEscape(sc, cx, false))
                            return setError();

                        Expression e0;
                        Expression.extractLast(e2x, e0);

                        auto e = Expression.combine(e0, ae, cx);
                        e = e.expressionSemantic(sc);
                        result = e;
                        return;
                    }
                    // https://issues.dlang.org/show_bug.cgi?id=21586
                    // Rewrite CondExp or e1 will miss direct construction, e.g.
                    // e1 = a ? S(1) : ...; -> AST: e1 = a ? (S(0)).this(1) : ...;
                    // a temporary created and an extra destructor call.
                    // AST will be rewritten to:
                    // a ? e1 = 0, e1.this(1) : ...; -> blitting plus construction
                    if (e2x.op == EXP.question)
                    {
                        /* Rewrite as:
                         *  a ? e1 = b : e1 = c;
                         */
                        CondExp econd = cast(CondExp)e2x;
                        Expression ea1 = new ConstructExp(econd.e1.loc, e1x, econd.e1);
                        Expression ea2 = new ConstructExp(econd.e2.loc, e1x, econd.e2);
                        Expression e = new CondExp(exp.loc, econd.econd, ea1, ea2);
                        result = e.expressionSemantic(sc);
                        return;
                    }
                    if (sd.postblit || sd.hasCopyCtor)
                    {
                        /* We have a copy constructor for this
                         */

                        if (e2x.isLvalue())
                        {
                            if (sd.hasCopyCtor)
                            {
                                /* Rewrite as:
                                 * e1 = init, e1.copyCtor(e2);
                                 */
                                Expression einit = new BlitExp(exp.loc, exp.e1, getInitExp(sd, exp.loc, sc, t1));
                                einit.type = e1x.type;

                                Expression e;
                                e = new DotIdExp(exp.loc, e1x, Id.ctor);
                                e = new CallExp(exp.loc, e, e2x);
                                e = new CommaExp(exp.loc, einit, e);

                                //printf("e: %s\n", e.toChars());

                                result = e.expressionSemantic(sc);
                                return;
                            }
                            else
                            {
                                if (!e2x.type.implicitConvTo(e1x.type))
                                {
                                    exp.error("conversion error from `%s` to `%s`",
                                        e2x.type.toChars(), e1x.type.toChars());
                                    return setError();
                                }

                                /* Rewrite as:
                                 *  (e1 = e2).postblit();
                                 *
                                 * Blit assignment e1 = e2 returns a reference to the original e1,
                                 * then call the postblit on it.
                                 */
                                Expression e = e1x.copy();
                                e.type = e.type.mutableOf();
                                if (e.type.isShared && !sd.type.isShared)
                                    e.type = e.type.unSharedOf();
                                e = new BlitExp(exp.loc, e, e2x);
                                e = new DotVarExp(exp.loc, e, sd.postblit, false);
                                e = new CallExp(exp.loc, e);
                                result = e.expressionSemantic(sc);
                                return;
                            }
                        }
                        else
                        {
                            /* The struct value returned from the function is transferred
                             * so should not call the destructor on it.
                             */
                            e2x = valueNoDtor(e2x);
                        }
                    }

                    // https://issues.dlang.org/show_bug.cgi?id=19251
                    // if e2 cannot be converted to e1.type, maybe there is an alias this
                    if (!e2x.implicitConvTo(t1))
                    {
                        AggregateDeclaration ad2 = isAggregate(e2x.type);
                        if (ad2 && ad2.aliasthis && !isRecursiveAliasThis(exp.att2, exp.e2.type))
                        {
                            /* Rewrite (e1 op e2) as:
                             *      (e1 op e2.aliasthis)
                             */
                            exp.e2 = new DotIdExp(exp.e2.loc, exp.e2, ad2.aliasthis.ident);
                            result = exp.expressionSemantic(sc);
                            return;
                        }
                    }
                }
                else if (!e2x.implicitConvTo(t1))
                {
                    sd.size(exp.loc);
                    if (sd.sizeok != Sizeok.done)
                        return setError();
                    if (!sd.ctor)
                        sd.ctor = sd.searchCtor();

                    if (sd.ctor)
                    {
                        /* Look for implicit constructor call
                         * Rewrite as:
                         *  e1 = init, e1.ctor(e2)
                         */

                        /* Fix Issue 5153 : https://issues.dlang.org/show_bug.cgi?id=5153
                         * Using `new` to initialize a struct object is a common mistake, but
                         * the error message from the compiler is not very helpful in that
                         * case. If exp.e2 is a NewExp and the type of new is the same as
                         * the type as exp.e1 (struct in this case), then we know for sure
                         * that the user wants to instantiate a struct. This is done to avoid
                         * issuing an error when the user actually wants to call a constructor
                         * which receives a class object.
                         *
                         * Foo f = new Foo2(0); is a valid expression if Foo has a constructor
                         * which receives an instance of a Foo2 class
                         */
                        if (exp.e2.op == EXP.new_)
                        {
                            auto newExp = cast(NewExp)(exp.e2);
                            if (newExp.newtype && newExp.newtype == t1)
                            {
                                error(exp.loc, "cannot implicitly convert expression `%s` of type `%s` to `%s`",
                                      newExp.toChars(), newExp.type.toChars(), t1.toChars());
                                errorSupplemental(exp.loc, "Perhaps remove the `new` keyword?");
                                return setError();
                            }
                        }

                        Expression einit = new BlitExp(exp.loc, e1x, getInitExp(sd, exp.loc, sc, t1));
                        einit.type = e1x.type;

                        Expression e;
                        e = new DotIdExp(exp.loc, e1x, Id.ctor);
                        e = new CallExp(exp.loc, e, e2x);
                        e = new CommaExp(exp.loc, einit, e);
                        e = e.expressionSemantic(sc);
                        result = e;
                        return;
                    }
                    if (search_function(sd, Id.call))
                    {
                        /* Look for static opCall
                         * https://issues.dlang.org/show_bug.cgi?id=2702
                         * Rewrite as:
                         *  e1 = typeof(e1).opCall(arguments)
                         */
                        e2x = typeDotIdExp(e2x.loc, e1x.type, Id.call);
                        e2x = new CallExp(exp.loc, e2x, exp.e2);

                        e2x = e2x.expressionSemantic(sc);
                        e2x = resolveProperties(sc, e2x);
                        if (e2x.op == EXP.error)
                        {
                            result = e2x;
                            return;
                        }
                        if (e2x.checkValue() || e2x.checkSharedAccess(sc))
                            return setError();
                    }
                }
                else // https://issues.dlang.org/show_bug.cgi?id=11355
                {
                    AggregateDeclaration ad2 = isAggregate(e2x.type);
                    if (ad2 && ad2.aliasthis && !isRecursiveAliasThis(exp.att2, exp.e2.type))
                    {
                        /* Rewrite (e1 op e2) as:
                         *      (e1 op e2.aliasthis)
                         */
                        exp.e2 = new DotIdExp(exp.e2.loc, exp.e2, ad2.aliasthis.ident);
                        result = exp.expressionSemantic(sc);
                        return;
                    }
                }
            }
            else if (exp.op == EXP.assign)
            {
                if (e1x.op == EXP.index && (cast(IndexExp)e1x).e1.type.toBasetype().ty == Taarray)
                {
                    /*
                     * Rewrite:
                     *      aa[key] = e2;
                     * as:
                     *      ref __aatmp = aa;
                     *      ref __aakey = key;
                     *      ref __aaval = e2;
                     *      (__aakey in __aatmp
                     *          ? __aatmp[__aakey].opAssign(__aaval)
                     *          : ConstructExp(__aatmp[__aakey], __aaval));
                     */
                    // ensure we keep the expr modifiable
                    Expression esetting = (cast(IndexExp)e1x).markSettingAAElem();
                    if (esetting.op == EXP.error)
                    {
                        result = esetting;
                        return;
                    }
                    assert(esetting.op == EXP.index);
                    IndexExp ie = cast(IndexExp) esetting;
                    Type t2 = e2x.type.toBasetype();

                    Expression e0 = null;
                    Expression ea = extractSideEffect(sc, "__aatmp", e0, ie.e1);
                    Expression ek = extractSideEffect(sc, "__aakey", e0, ie.e2);
                    Expression ev = extractSideEffect(sc, "__aaval", e0, e2x);

                    AssignExp ae = cast(AssignExp)exp.copy();
                    ae.e1 = new IndexExp(exp.loc, ea, ek);
                    ae.e1 = ae.e1.expressionSemantic(sc);
                    ae.e1 = ae.e1.optimize(WANTvalue);
                    ae.e2 = ev;
                    Expression e = ae.op_overload(sc);
                    if (e)
                    {
                        Expression ey = null;
                        if (t2.ty == Tstruct && sd == t2.toDsymbol(sc))
                        {
                            ey = ev;
                        }
                        else if (!ev.implicitConvTo(ie.type) && sd.ctor)
                        {
                            // Look for implicit constructor call
                            // Rewrite as S().ctor(e2)
                            ey = new StructLiteralExp(exp.loc, sd, null);
                            ey = new DotIdExp(exp.loc, ey, Id.ctor);
                            ey = new CallExp(exp.loc, ey, ev);
                            ey = ey.trySemantic(sc);
                        }
                        if (ey)
                        {
                            Expression ex;
                            ex = new IndexExp(exp.loc, ea, ek);
                            ex = ex.expressionSemantic(sc);
                            ex = ex.modifiableLvalue(sc, ex); // allocate new slot
                            ex = ex.optimize(WANTvalue);

                            ey = new ConstructExp(exp.loc, ex, ey);
                            ey = ey.expressionSemantic(sc);
                            if (ey.op == EXP.error)
                            {
                                result = ey;
                                return;
                            }
                            ex = e;

                            // https://issues.dlang.org/show_bug.cgi?id=14144
                            // The whole expression should have the common type
                            // of opAssign() return and assigned AA entry.
                            // Even if there's no common type, expression should be typed as void.
                            if (!typeMerge(sc, EXP.question, ex, ey))
                            {
                                ex = new CastExp(ex.loc, ex, Type.tvoid);
                                ey = new CastExp(ey.loc, ey, Type.tvoid);
                            }
                            e = new CondExp(exp.loc, new InExp(exp.loc, ek, ea), ex, ey);
                        }
                        e = Expression.combine(e0, e);
                        e = e.expressionSemantic(sc);
                        result = e;
                        return;
                    }
                }
                else
                {
                    Expression e = exp.op_overload(sc);
                    if (e)
                    {
                        result = e;
                        return;
                    }
                }
            }
            else
                assert(exp.op == EXP.blit);

            if (e2x.checkValue())
                return setError();

            exp.e1 = e1x;
            exp.e2 = e2x;
        }
        else if (t1.ty == Tclass)
        {
            // Disallow assignment operator overloads for same type
            if (exp.op == EXP.assign && !exp.e2.implicitConvTo(exp.e1.type))
            {
                Expression e = exp.op_overload(sc);
                if (e)
                {
                    result = e;
                    return;
                }
            }
            if (exp.e2.checkValue())
                return setError();
        }
        else if (t1.ty == Tsarray)
        {
            // SliceExp cannot have static array type without context inference.
            assert(exp.e1.op != EXP.slice);
            Expression e1x = exp.e1;
            Expression e2x = exp.e2;

            /* C strings come through as static arrays. May need to adjust the size of the
             * string to match the size of e1.
             */
            Type t2 = e2x.type.toBasetype();
            if (sc.flags & SCOPE.Cfile && e2x.isStringExp() && t2.isTypeSArray())
            {
                uinteger_t dim1 = t1.isTypeSArray().dim.toInteger();
                uinteger_t dim2 = t2.isTypeSArray().dim.toInteger();
                if (dim1 + 1 == dim2 || dim2 < dim1)
                {
                    auto tsa2 = t2.isTypeSArray();
                    auto newt = tsa2.next.sarrayOf(dim1).immutableOf();
                    e2x = castTo(e2x, sc, newt);
                    exp.e2 = e2x;
                }
            }

            if (e2x.implicitConvTo(e1x.type))
            {
                if (exp.op != EXP.blit && (e2x.op == EXP.slice && (cast(UnaExp)e2x).e1.isLvalue() || e2x.op == EXP.cast_ && (cast(UnaExp)e2x).e1.isLvalue() || e2x.op != EXP.slice && e2x.isLvalue()))
                {
                    if (e1x.checkPostblit(sc, t1))
                        return setError();
                }

                // e2 matches to t1 because of the implicit length match, so
                if (isUnaArrayOp(e2x.op) || isBinArrayOp(e2x.op))
                {
                    // convert e1 to e1[]
                    // e.g. e1[] = a[] + b[];
                    auto sle = new SliceExp(e1x.loc, e1x, null, null);
                    sle.arrayop = true;
                    e1x = sle.expressionSemantic(sc);
                }
                else
                {
                    // convert e2 to t1 later
                    // e.g. e1 = [1, 2, 3];
                }
            }
            else
            {
                if (e2x.implicitConvTo(t1.nextOf().arrayOf()) > MATCH.nomatch)
                {
                    uinteger_t dim1 = (cast(TypeSArray)t1).dim.toInteger();
                    uinteger_t dim2 = dim1;
                    if (auto ale = e2x.isArrayLiteralExp())
                    {
                        dim2 = ale.elements ? ale.elements.length : 0;
                    }
                    else if (auto se = e2x.isSliceExp())
                    {
                        Type tx = toStaticArrayType(se);
                        if (tx)
                            dim2 = (cast(TypeSArray)tx).dim.toInteger();
                    }
                    if (dim1 != dim2)
                    {
                        exp.error("mismatched array lengths, %d and %d", cast(int)dim1, cast(int)dim2);
                        return setError();
                    }
                }

                // May be block or element-wise assignment, so
                // convert e1 to e1[]
                if (exp.op != EXP.assign)
                {
                    // If multidimensional static array, treat as one large array
                    //
                    // Find the appropriate array type depending on the assignment, e.g.
                    // int[3] = int => int[3]
                    // int[3][2] = int => int[6]
                    // int[3][2] = int[] => int[3][2]
                    // int[3][2][4] + int => int[24]
                    // int[3][2][4] + int[] => int[3][8]
                    ulong dim = t1.isTypeSArray().dim.toUInteger();
                    auto type = t1.nextOf();

                    for (TypeSArray tsa; (tsa = type.isTypeSArray()) !is null; )
                    {
                        import core.checkedint : mulu;

                        // Accumulate skipped dimensions
                        bool overflow = false;
                        dim = mulu(dim, tsa.dim.toUInteger(), overflow);
                        if (overflow || dim >= uint.max)
                        {
                            // dym exceeds maximum array size
                            exp.error("static array `%s` size overflowed to %llu",
                                        e1x.type.toChars(), cast(ulong) dim);
                            return setError();
                        }

                        // Move to the element type
                        type = tsa.nextOf().toBasetype();

                        // Rewrite ex1 as a static array if a matching type was found
                        if (e2x.implicitConvTo(type) > MATCH.nomatch)
                        {
                            e1x.type = type.sarrayOf(dim);
                            break;
                        }
                    }
                }
                auto sle = new SliceExp(e1x.loc, e1x, null, null);
                sle.arrayop = true;
                e1x = sle.expressionSemantic(sc);
            }
            if (e1x.op == EXP.error)
                return setResult(e1x);
            if (e2x.op == EXP.error)
                return setResult(e2x);

            exp.e1 = e1x;
            exp.e2 = e2x;
            t1 = e1x.type.toBasetype();
        }
        /* Check the mutability of e1.
         */
        if (auto ale = exp.e1.isArrayLengthExp())
        {
            // e1 is not an lvalue, but we let code generator handle it

            auto ale1x = ale.e1.modifiableLvalue(sc, exp.e1);
            if (ale1x.op == EXP.error)
                return setResult(ale1x);
            ale.e1 = ale1x;

            Type tn = ale.e1.type.toBasetype().nextOf();
            checkDefCtor(ale.loc, tn);

            Identifier hook = global.params.tracegc ? Id._d_arraysetlengthTTrace : Id._d_arraysetlengthT;
            if (!verifyHookExist(exp.loc, *sc, Id._d_arraysetlengthTImpl, "resizing arrays"))
                return setError();

            exp.e2 = exp.e2.expressionSemantic(sc);
            auto lc = lastComma(exp.e2);
            lc = lc.optimize(WANTvalue);
            // use slice expression when arr.length = 0 to avoid runtime call
            if(lc.op == EXP.int64 && lc.toInteger() == 0)
            {
                Expression se = new SliceExp(ale.loc, ale.e1, lc, lc);
                Expression as = new AssignExp(ale.loc, ale.e1, se);
                as = as.expressionSemantic(sc);
                auto res = Expression.combine(as, exp.e2);
                res.type = ale.type;
                return setResult(res);
            }

            // Lower to object._d_arraysetlengthTImpl!(typeof(e1))._d_arraysetlengthT{,Trace}(e1, e2)
            Expression id = new IdentifierExp(ale.loc, Id.empty);
            id = new DotIdExp(ale.loc, id, Id.object);
            auto tiargs = new Objects();
            tiargs.push(ale.e1.type);
            id = new DotTemplateInstanceExp(ale.loc, id, Id._d_arraysetlengthTImpl, tiargs);
            id = new DotIdExp(ale.loc, id, hook);
            id = id.expressionSemantic(sc);

            auto arguments = new Expressions();
            arguments.reserve(5);
            if (global.params.tracegc)
            {
                auto funcname = (sc.callsc && sc.callsc.func) ? sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
                arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
                arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
                arguments.push(new StringExp(exp.loc, funcname.toDString()));
            }
            arguments.push(ale.e1);
            arguments.push(exp.e2);

            Expression ce = new CallExp(ale.loc, id, arguments);
            auto res = ce.expressionSemantic(sc);
            // if (global.params.verbose)
            //     message("lowered   %s =>\n          %s", exp.toChars(), res.toChars());
            return setResult(res);
        }
        else if (auto se = exp.e1.isSliceExp())
        {
            Type tn = se.type.nextOf();
            const fun = sc.func;
            if (exp.op == EXP.assign && !tn.isMutable() &&
                // allow modifiation in module ctor, see
                // https://issues.dlang.org/show_bug.cgi?id=9884
                (!fun || (fun && !fun.isStaticCtorDeclaration())))
            {
                exp.error("slice `%s` is not mutable", se.toChars());
                return setError();
            }

            if (exp.op == EXP.assign && !tn.baseElemOf().isAssignable())
            {
                exp.error("slice `%s` is not mutable, struct `%s` has immutable members",
                    exp.e1.toChars(), tn.baseElemOf().toChars());
                result = ErrorExp.get();
                return;
            }

            // For conditional operator, both branches need conversion.
            while (se.e1.op == EXP.slice)
                se = cast(SliceExp)se.e1;
            if (se.e1.op == EXP.question && se.e1.type.toBasetype().ty == Tsarray)
            {
                se.e1 = se.e1.modifiableLvalue(sc, exp.e1);
                if (se.e1.op == EXP.error)
                    return setResult(se.e1);
            }
        }
        else
        {
            if (t1.ty == Tsarray && exp.op == EXP.assign)
            {
                Type tn = exp.e1.type.nextOf();
                if (tn && !tn.baseElemOf().isAssignable())
                {
                    exp.error("array `%s` is not mutable, struct `%s` has immutable members",
                        exp.e1.toChars(), tn.baseElemOf().toChars());
                    result = ErrorExp.get();
                    return;
                }
            }

            Expression e1x = exp.e1;

            // Try to do a decent error message with the expression
            // before it gets constant folded
            if (exp.op == EXP.assign)
                e1x = e1x.modifiableLvalue(sc, e1old);

            e1x = e1x.optimize(WANTvalue, /*keepLvalue*/ true);

            if (e1x.op == EXP.error)
            {
                result = e1x;
                return;
            }
            exp.e1 = e1x;
        }

        /* Tweak e2 based on the type of e1.
         */
        Expression e2x = exp.e2;
        Type t2 = e2x.type.toBasetype();

        // If it is a array, get the element type. Note that it may be
        // multi-dimensional.
        Type telem = t1;
        while (telem.ty == Tarray)
            telem = telem.nextOf();

        if (exp.e1.op == EXP.slice && t1.nextOf() &&
            (telem.ty != Tvoid || e2x.op == EXP.null_) &&
            e2x.implicitConvTo(t1.nextOf()))
        {
            // Check for block assignment. If it is of type void[], void[][], etc,
            // '= null' is the only allowable block assignment (Bug 7493)
            exp.memset = MemorySet.blockAssign;    // make it easy for back end to tell what this is
            e2x = e2x.implicitCastTo(sc, t1.nextOf());
            if (exp.op != EXP.blit && e2x.isLvalue() && exp.e1.checkPostblit(sc, t1.nextOf()))
                return setError();
        }
        else if (exp.e1.op == EXP.slice &&
                 (t2.ty == Tarray || t2.ty == Tsarray) &&
                 t2.nextOf().implicitConvTo(t1.nextOf()))
        {
            // Check element-wise assignment.

            /* If assigned elements number is known at compile time,
             * check the mismatch.
             */
            SliceExp se1 = cast(SliceExp)exp.e1;
            TypeSArray tsa1 = cast(TypeSArray)toStaticArrayType(se1);
            TypeSArray tsa2 = null;
            if (auto ale = e2x.isArrayLiteralExp())
                tsa2 = cast(TypeSArray)t2.nextOf().sarrayOf(ale.elements.length);
            else if (auto se = e2x.isSliceExp())
                tsa2 = cast(TypeSArray)toStaticArrayType(se);
            else
                tsa2 = t2.isTypeSArray();

            if (tsa1 && tsa2)
            {
                uinteger_t dim1 = tsa1.dim.toInteger();
                uinteger_t dim2 = tsa2.dim.toInteger();
                if (dim1 != dim2)
                {
                    exp.error("mismatched array lengths %d and %d for assignment `%s`", cast(int)dim1, cast(int)dim2, exp.toChars());
                    return setError();
                }
            }

            if (exp.op != EXP.blit &&
                (e2x.op == EXP.slice && (cast(UnaExp)e2x).e1.isLvalue() ||
                 e2x.op == EXP.cast_ && (cast(UnaExp)e2x).e1.isLvalue() ||
                 e2x.op != EXP.slice && e2x.isLvalue()))
            {
                if (exp.e1.checkPostblit(sc, t1.nextOf()))
                    return setError();
            }

            if (0 && global.params.warnings != DiagnosticReporting.off && !global.gag && exp.op == EXP.assign &&
                e2x.op != EXP.slice && e2x.op != EXP.assign &&
                e2x.op != EXP.arrayLiteral && e2x.op != EXP.string_ &&
                !(e2x.op == EXP.add || e2x.op == EXP.min ||
                  e2x.op == EXP.mul || e2x.op == EXP.div ||
                  e2x.op == EXP.mod || e2x.op == EXP.xor ||
                  e2x.op == EXP.and || e2x.op == EXP.or ||
                  e2x.op == EXP.pow ||
                  e2x.op == EXP.tilde || e2x.op == EXP.negate))
            {
                const(char)* e1str = exp.e1.toChars();
                const(char)* e2str = e2x.toChars();
                exp.warning("explicit element-wise assignment `%s = (%s)[]` is better than `%s = %s`", e1str, e2str, e1str, e2str);
            }

            Type t2n = t2.nextOf();
            Type t1n = t1.nextOf();
            int offset;
            if (t2n.equivalent(t1n) ||
                t1n.isBaseOf(t2n, &offset) && offset == 0)
            {
                /* Allow copy of distinct qualifier elements.
                 * eg.
                 *  char[] dst;  const(char)[] src;
                 *  dst[] = src;
                 *
                 *  class C {}   class D : C {}
                 *  C[2] ca;  D[] da;
                 *  ca[] = da;
                 */
                if (isArrayOpValid(e2x))
                {
                    // Don't add CastExp to keep AST for array operations
                    e2x = e2x.copy();
                    e2x.type = exp.e1.type.constOf();
                }
                else
                    e2x = e2x.castTo(sc, exp.e1.type.constOf());
            }
            else
            {
                /* https://issues.dlang.org/show_bug.cgi?id=15778
                 * A string literal has an array type of immutable
                 * elements by default, and normally it cannot be convertible to
                 * array type of mutable elements. But for element-wise assignment,
                 * elements need to be const at best. So we should give a chance
                 * to change code unit size for polysemous string literal.
                 */
                if (e2x.op == EXP.string_)
                    e2x = e2x.implicitCastTo(sc, exp.e1.type.constOf());
                else
                    e2x = e2x.implicitCastTo(sc, exp.e1.type);
            }
            if (t1n.toBasetype.ty == Tvoid && t2n.toBasetype.ty == Tvoid)
            {
                if (sc.setUnsafe(false, exp.loc, "cannot copy `void[]` to `void[]` in `@safe` code"))
                    return setError();
            }
        }
        else
        {
            if (0 && global.params.warnings != DiagnosticReporting.off && !global.gag && exp.op == EXP.assign &&
                t1.ty == Tarray && t2.ty == Tsarray &&
                e2x.op != EXP.slice &&
                t2.implicitConvTo(t1))
            {
                // Disallow ar[] = sa (Converted to ar[] = sa[])
                // Disallow da   = sa (Converted to da   = sa[])
                const(char)* e1str = exp.e1.toChars();
                const(char)* e2str = e2x.toChars();
                const(char)* atypestr = exp.e1.op == EXP.slice ? "element-wise" : "slice";
                exp.warning("explicit %s assignment `%s = (%s)[]` is better than `%s = %s`", atypestr, e1str, e2str, e1str, e2str);
            }
            if (exp.op == EXP.blit)
                e2x = e2x.castTo(sc, exp.e1.type);
            else
            {
                e2x = e2x.implicitCastTo(sc, exp.e1.type);

                // Fix Issue 13435: https://issues.dlang.org/show_bug.cgi?id=13435

                // If the implicit cast has failed and the assign expression is
                // the initialization of a struct member field
                if (e2x.op == EXP.error && exp.op == EXP.construct && t1.ty == Tstruct)
                {
                    scope sd = (cast(TypeStruct)t1).sym;
                    Dsymbol opAssign = search_function(sd, Id.assign);

                    // and the struct defines an opAssign
                    if (opAssign)
                    {
                        // offer more information about the cause of the problem
                        errorSupplemental(exp.loc,
                                          "`%s` is the first assignment of `%s` therefore it represents its initialization",
                                          exp.toChars(), exp.e1.toChars());
                        errorSupplemental(exp.loc,
                                          "`opAssign` methods are not used for initialization, but for subsequent assignments");
                    }
                }
            }
        }
        if (e2x.op == EXP.error)
        {
            result = e2x;
            return;
        }
        exp.e2 = e2x;
        t2 = exp.e2.type.toBasetype();

        /* Look for array operations
         */
        if ((t2.ty == Tarray || t2.ty == Tsarray) && isArrayOpValid(exp.e2))
        {
            // Look for valid array operations
            if (exp.memset != MemorySet.blockAssign &&
                exp.e1.op == EXP.slice &&
                (isUnaArrayOp(exp.e2.op) || isBinArrayOp(exp.e2.op)))
            {
                exp.type = exp.e1.type;
                if (exp.op == EXP.construct) // https://issues.dlang.org/show_bug.cgi?id=10282
                                        // tweak mutability of e1 element
                    exp.e1.type = exp.e1.type.nextOf().mutableOf().arrayOf();
                result = arrayOp(exp, sc);
                return;
            }

            // Drop invalid array operations in e2
            //  d = a[] + b[], d = (a[] + b[])[0..2], etc
            if (checkNonAssignmentArrayOp(exp.e2, exp.memset != MemorySet.blockAssign && exp.op == EXP.assign))
                return setError();

            // Remains valid array assignments
            //  d = d[], d = [1,2,3], etc
        }

        /* Don't allow assignment to classes that were allocated on the stack with:
         *      scope Class c = new Class();
         */
        if (exp.e1.op == EXP.variable && exp.op == EXP.assign)
        {
            VarExp ve = cast(VarExp)exp.e1;
            VarDeclaration vd = ve.var.isVarDeclaration();
            if (vd && vd.onstack)
            {
                assert(t1.ty == Tclass);
                exp.error("cannot rebind scope variables");
            }
        }

        if (exp.e1.op == EXP.variable && (cast(VarExp)exp.e1).var.ident == Id.ctfe)
        {
            exp.error("cannot modify compiler-generated variable `__ctfe`");
        }

        exp.type = exp.e1.type;
        assert(exp.type);
        auto assignElem = exp.e2;
        auto res = exp.op == EXP.assign ? exp.reorderSettingAAElem(sc) : exp;
        /* https://issues.dlang.org/show_bug.cgi?id=22366
         *
         * `reorderSettingAAElem` creates a tree of comma expressions, however,
         * `checkAssignExp` expects only AssignExps.
         */
        if (res == exp) // no `AA[k] = v` rewrite was performed
            checkAssignEscape(sc, res, false, false);
        else
            checkNewEscape(sc, assignElem, false); // assigning to AA puts it on heap

        if (auto ae = res.isConstructExp())
        {
            Type t1b = ae.e1.type.toBasetype();
            if (t1b.ty != Tsarray && t1b.ty != Tarray)
                return setResult(res);

            // only non-trivial array constructions may need to be lowered (non-POD elements basically)
            Type t1e = t1b.nextOf();
            TypeStruct ts = t1e.baseElemOf().isTypeStruct();
            if (!ts || (!ts.sym.postblit && !ts.sym.hasCopyCtor && !ts.sym.dtor))
                return setResult(res);

            // don't lower ref-constructions etc.
            if (!(t1b.ty == Tsarray || ae.e1.isSliceExp) ||
                (ae.e1.isVarExp && ae.e1.isVarExp.var.isVarDeclaration.isReference))
                return setResult(res);

            // Construction from an equivalent other array?
            // Only lower with lvalue RHS elements; let the glue layer move rvalue elements.
            Type t2b = ae.e2.type.toBasetype();
            // skip over a (possibly implicit) cast of a static array RHS to a slice
            Expression rhs = ae.e2;
            Type rhsType = t2b;
            if (t2b.ty == Tarray)
            {
                if (auto ce = rhs.isCastExp())
                {
                    auto ct = ce.e1.type.toBasetype();
                    if (ct.ty == Tsarray)
                    {
                        rhs = ce.e1;
                        rhsType = ct;
                    }
                }
            }
            const lowerToArrayCtor =
                ( (rhsType.ty == Tarray && !rhs.isArrayLiteralExp) ||
                  (rhsType.ty == Tsarray && rhs.isLvalue) ) &&
                t1e.equivalent(t2b.nextOf);

            // Construction from a single element?
            // If the RHS is an rvalue, then we'll need to make a temporary for it (copied multiple times).
            const lowerToArraySetCtor = !lowerToArrayCtor && t1e.equivalent(t2b);

            if (lowerToArrayCtor || lowerToArraySetCtor)
            {
                auto func = lowerToArrayCtor ? Id._d_arrayctor : Id._d_arraysetctor;
                const other = lowerToArrayCtor ? "other array" : "value";
                if (!verifyHookExist(exp.loc, *sc, func, "construct array with " ~ other, Id.object))
                    return setError();

                // Lower to object._d_array{,set}ctor(e1, e2)
                Expression id = new IdentifierExp(exp.loc, Id.empty);
                id = new DotIdExp(exp.loc, id, Id.object);
                id = new DotIdExp(exp.loc, id, func);

                auto arguments = new Expressions();
                arguments.push(new CastExp(ae.loc, ae.e1, t1e.arrayOf).expressionSemantic(sc));
                if (lowerToArrayCtor)
                {
                    arguments.push(new CastExp(ae.loc, rhs, t2b.nextOf.arrayOf).expressionSemantic(sc));
                    Expression ce = new CallExp(exp.loc, id, arguments);
                    res = ce.expressionSemantic(sc);
                }
                else
                {
                    Expression e0;
                    // promote an rvalue RHS element to a temporary, it's passed by ref to _d_arraysetctor
                    if (!ae.e2.isLvalue)
                    {
                        auto vd = copyToTemp(STC.scope_, "__setctor", ae.e2);
                        e0 = new DeclarationExp(vd.loc, vd).expressionSemantic(sc);
                        arguments.push(new VarExp(vd.loc, vd).expressionSemantic(sc));
                    }
                    else
                        arguments.push(ae.e2);

                    Expression ce = new CallExp(exp.loc, id, arguments);
                    res = Expression.combine(e0, ce).expressionSemantic(sc);
                }

                if (global.params.verbose)
                    message("lowered   %s =>\n          %s", exp.toChars(), res.toChars());
            }
        }
        else if (auto ae = res.isAssignExp())
            res = lowerArrayAssign(ae);
        else if (auto ce = res.isCommaExp())
        {
            if (auto ae1 = ce.e1.isAssignExp())
                ce.e1 = lowerArrayAssign(ae1, true);
            if (auto ae2 = ce.e2.isAssignExp())
                ce.e2 = lowerArrayAssign(ae2, true);
        }

        return setResult(res);
    }

    /***************************************
     * Lower AssignExp to `_d_array{setassign,assign_l,assign_r}` if needed.
     *
     * Params:
     *      ae = the AssignExp to be lowered
     *      fromCommaExp = indicates whether `ae` is part of a CommaExp or not,
     *                     so no unnecessary temporay variable is created.
     * Returns:
     *      a CommaExp contiaining call a to `_d_array{setassign,assign_l,assign_r}`
     *      if needed or `ae` otherwise
     */
    private Expression lowerArrayAssign(AssignExp ae, bool fromCommaExp = false)
    {
        Type t1b = ae.e1.type.toBasetype();
        if (t1b.ty != Tsarray && t1b.ty != Tarray)
            return ae;

        const isArrayAssign = (ae.e1.isSliceExp() || ae.e1.type.ty == Tsarray) &&
            (ae.e2.type.ty == Tsarray || ae.e2.type.ty == Tarray) &&
            (ae.e1.type.nextOf() && ae.e2.type.nextOf() && ae.e1.type.nextOf.mutableOf.equals(ae.e2.type.nextOf.mutableOf()));

        const isArraySetAssign = (ae.e1.isSliceExp() || ae.e1.type.ty == Tsarray) &&
            (ae.e1.type.nextOf() && ae.e2.type.implicitConvTo(ae.e1.type.nextOf()));

        if (!isArrayAssign && !isArraySetAssign)
            return ae;

        const ts = t1b.nextOf().baseElemOf().isTypeStruct();
        if (!ts || (!ts.sym.postblit && !ts.sym.dtor))
            return ae;

        Expression res;
        Identifier func = isArraySetAssign ? Id._d_arraysetassign :
            ae.e2.isLvalue() || ae.e2.isSliceExp() ? Id._d_arrayassign_l : Id._d_arrayassign_r;

        // Lower to `.object._d_array{setassign,assign_l,assign_r}(e1, e2)``
        Expression id = new IdentifierExp(ae.loc, Id.empty);
        id = new DotIdExp(ae.loc, id, Id.object);
        id = new DotIdExp(ae.loc, id, func);

        auto arguments = new Expressions();
        arguments.push(new CastExp(ae.loc, ae.e1, ae.e1.type.nextOf.arrayOf)
            .expressionSemantic(sc));

        Expression eValue2, value2 = ae.e2;
        if (isArrayAssign && value2.isLvalue())
            value2 = new CastExp(ae.loc, ae.e2, ae.e2.type.nextOf.arrayOf())
                .expressionSemantic(sc);
        else if (!fromCommaExp &&
            (isArrayAssign || (isArraySetAssign && !value2.isLvalue())))
        {
            // Rvalues from CommaExps were introduced in `visit(AssignExp)`
            // and are temporary variables themselves. Rvalues from trivial
            // SliceExps are simply passed by reference without any copying.

            // `__assigntmp` will be destroyed together with the array `ae.e1`.
            // When `ae.e2` is a variadic arg array, it is also `scope`, so
            // `__assigntmp` may also be scope.
            StorageClass stc = STC.nodtor;
            if (isArrayAssign)
                stc |= STC.rvalue | STC.scope_;

            auto vd = copyToTemp(stc, "__assigntmp", ae.e2);
            eValue2 = new DeclarationExp(vd.loc, vd).expressionSemantic(sc);
            value2 = new VarExp(vd.loc, vd).expressionSemantic(sc);
        }
        arguments.push(value2);

        Expression ce = new CallExp(ae.loc, id, arguments);
        res = Expression.combine(eValue2, ce).expressionSemantic(sc);
        if (isArrayAssign)
            res = Expression.combine(res, ae.e1).expressionSemantic(sc);

        if (global.params.verbose)
            message("lowered   %s =>\n          %s", ae.toChars(), res.toChars());

        return res;
    }

    override void visit(PowAssignExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.e1.checkReadModifyWrite(exp.op, exp.e2))
            return setError();

        assert(exp.e1.type && exp.e2.type);
        if (exp.e1.op == EXP.slice || exp.e1.type.ty == Tarray || exp.e1.type.ty == Tsarray)
        {
            if (checkNonAssignmentArrayOp(exp.e1))
                return setError();

            // T[] ^^= ...
            if (exp.e2.implicitConvTo(exp.e1.type.nextOf()))
            {
                // T[] ^^= T
                exp.e2 = exp.e2.castTo(sc, exp.e1.type.nextOf());
            }
            else if (Expression ex = typeCombine(exp, sc))
            {
                result = ex;
                return;
            }

            // Check element types are arithmetic
            Type tb1 = exp.e1.type.nextOf().toBasetype();
            Type tb2 = exp.e2.type.toBasetype();
            if (tb2.ty == Tarray || tb2.ty == Tsarray)
                tb2 = tb2.nextOf().toBasetype();
            if ((tb1.isintegral() || tb1.isfloating()) && (tb2.isintegral() || tb2.isfloating()))
            {
                exp.type = exp.e1.type;
                result = arrayOp(exp, sc);
                return;
            }
        }
        else
        {
            exp.e1 = exp.e1.modifiableLvalue(sc, exp.e1);
        }

        if ((exp.e1.type.isintegral() || exp.e1.type.isfloating()) && (exp.e2.type.isintegral() || exp.e2.type.isfloating()))
        {
            Expression e0 = null;
            e = exp.reorderSettingAAElem(sc);
            e = Expression.extractLast(e, e0);
            assert(e == exp);

            if (exp.e1.op == EXP.variable)
            {
                // Rewrite: e1 = e1 ^^ e2
                e = new PowExp(exp.loc, exp.e1.syntaxCopy(), exp.e2);
                e = new AssignExp(exp.loc, exp.e1, e);
            }
            else
            {
                // Rewrite: ref tmp = e1; tmp = tmp ^^ e2
                auto v = copyToTemp(STC.ref_, "__powtmp", exp.e1);
                auto de = new DeclarationExp(exp.e1.loc, v);
                auto ve = new VarExp(exp.e1.loc, v);
                e = new PowExp(exp.loc, ve, exp.e2);
                e = new AssignExp(exp.loc, new VarExp(exp.e1.loc, v), e);
                e = new CommaExp(exp.loc, de, e);
            }
            e = Expression.combine(e0, e);
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }
        result = exp.incompatibleTypes();
    }

    override void visit(CatAssignExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        //printf("CatAssignExp::semantic() %s\n", exp.toChars());
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (SliceExp se = exp.e1.isSliceExp())
        {
            if (se.e1.type.toBasetype().ty == Tsarray)
            {
                exp.error("cannot append to static array `%s`", se.e1.type.toChars());
                return setError();
            }
        }

        exp.e1 = exp.e1.modifiableLvalue(sc, exp.e1);
        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        if (exp.e2.op == EXP.error)
        {
            result = exp.e2;
            return;
        }

        if (checkNonAssignmentArrayOp(exp.e2))
            return setError();

        Type tb1 = exp.e1.type.toBasetype();
        Type tb1next = tb1.nextOf();
        Type tb2 = exp.e2.type.toBasetype();

        /* Possibilities:
         * EXP.concatenateAssign: appending T[] to T[]
         * EXP.concatenateElemAssign: appending T to T[]
         * EXP.concatenateDcharAssign: appending dchar to T[]
         */
        if ((tb1.ty == Tarray) &&
            (tb2.ty == Tarray || tb2.ty == Tsarray) &&
            (exp.e2.implicitConvTo(exp.e1.type) ||
             (tb2.nextOf().implicitConvTo(tb1next) &&
              (tb2.nextOf().size(Loc.initial) == tb1next.size(Loc.initial)))))
        {
            // EXP.concatenateAssign
            assert(exp.op == EXP.concatenateAssign);
            if (exp.e1.checkPostblit(sc, tb1next))
                return setError();

            exp.e2 = exp.e2.castTo(sc, exp.e1.type);
        }
        else if ((tb1.ty == Tarray) && exp.e2.implicitConvTo(tb1next))
        {
            /* https://issues.dlang.org/show_bug.cgi?id=19782
             *
             * If e2 is implicitly convertible to tb1next, the conversion
             * might be done through alias this, in which case, e2 needs to
             * be modified accordingly (e2 => e2.aliasthis).
             */
            if (tb2.ty == Tstruct && (cast(TypeStruct)tb2).implicitConvToThroughAliasThis(tb1next))
                goto Laliasthis;
            if (tb2.ty == Tclass && (cast(TypeClass)tb2).implicitConvToThroughAliasThis(tb1next))
                goto Laliasthis;
            // Append element
            if (exp.e2.checkPostblit(sc, tb2))
                return setError();

            if (checkNewEscape(sc, exp.e2, false))
                return setError();

            exp = new CatElemAssignExp(exp.loc, exp.type, exp.e1, exp.e2.castTo(sc, tb1next));
            exp.e2 = doCopyOrMove(sc, exp.e2);
        }
        else if (tb1.ty == Tarray &&
                 (tb1next.ty == Tchar || tb1next.ty == Twchar) &&
                 exp.e2.type.ty != tb1next.ty &&
                 exp.e2.implicitConvTo(Type.tdchar))
        {
            // Append dchar to char[] or wchar[]
            exp = new CatDcharAssignExp(exp.loc, exp.type, exp.e1, exp.e2.castTo(sc, Type.tdchar));

            /* Do not allow appending wchar to char[] because if wchar happens
             * to be a surrogate pair, nothing good can result.
             */
        }
        else
        {
            // Try alias this on first operand
            static Expression tryAliasThisForLhs(BinAssignExp exp, Scope* sc)
            {
                AggregateDeclaration ad1 = isAggregate(exp.e1.type);
                if (!ad1 || !ad1.aliasthis)
                    return null;

                /* Rewrite (e1 op e2) as:
                 *      (e1.aliasthis op e2)
                 */
                if (isRecursiveAliasThis(exp.att1, exp.e1.type))
                    return null;
                //printf("att %s e1 = %s\n", Token.toChars(e.op), e.e1.type.toChars());
                Expression e1 = new DotIdExp(exp.loc, exp.e1, ad1.aliasthis.ident);
                BinExp be = cast(BinExp)exp.copy();
                be.e1 = e1;
                return be.trySemantic(sc);
            }

            // Try alias this on second operand
            static Expression tryAliasThisForRhs(BinAssignExp exp, Scope* sc)
            {
                AggregateDeclaration ad2 = isAggregate(exp.e2.type);
                if (!ad2 || !ad2.aliasthis)
                    return null;
                /* Rewrite (e1 op e2) as:
                 *      (e1 op e2.aliasthis)
                 */
                if (isRecursiveAliasThis(exp.att2, exp.e2.type))
                    return null;
                //printf("att %s e2 = %s\n", Token.toChars(e.op), e.e2.type.toChars());
                Expression e2 = new DotIdExp(exp.loc, exp.e2, ad2.aliasthis.ident);
                BinExp be = cast(BinExp)exp.copy();
                be.e2 = e2;
                return be.trySemantic(sc);
            }

    Laliasthis:
            result = tryAliasThisForLhs(exp, sc);
            if (result)
                return;

            result = tryAliasThisForRhs(exp, sc);
            if (result)
                return;

            exp.error("cannot append type `%s` to type `%s`", tb2.toChars(), tb1.toChars());
            return setError();
        }

        if (exp.e2.checkValue() || exp.e2.checkSharedAccess(sc))
            return setError();

        exp.type = exp.e1.type;
        auto assignElem = exp.e2;
        auto res = exp.reorderSettingAAElem(sc);
        if (res != exp) // `AA[k] = v` rewrite was performed
            checkNewEscape(sc, assignElem, false);
        else if (exp.op == EXP.concatenateElemAssign || exp.op == EXP.concatenateDcharAssign)
            checkAssignEscape(sc, res, false, false);

        result = res;

        if ((exp.op == EXP.concatenateAssign || exp.op == EXP.concatenateElemAssign) &&
            !(sc.flags & (SCOPE.ctfe | SCOPE.compile)))
        {
            // if aa ordering is triggered, `res` will be a CommaExp
            // and `.e2` will be the rewritten original expression.

            // `output` will point to the expression that the lowering will overwrite
            Expression* output;
            if (auto comma = res.isCommaExp())
            {
                output = &comma.e2;
                // manual cast because it could be either CatAssignExp or CatElemAssignExp
                exp = cast(CatAssignExp)comma.e2;
            }
            else
            {
                output = &result;
                exp = cast(CatAssignExp)result;
            }

            if (exp.op == EXP.concatenateAssign)
            {
                Identifier hook = global.params.tracegc ? Id._d_arrayappendTTrace : Id._d_arrayappendT;

                if (!verifyHookExist(exp.loc, *sc, hook, "appending array to arrays", Id.object))
                    return setError();

                // Lower to object._d_arrayappendT{,Trace}({file, line, funcname}, e1, e2)
                Expression id = new IdentifierExp(exp.loc, Id.empty);
                id = new DotIdExp(exp.loc, id, Id.object);
                id = new DotIdExp(exp.loc, id, hook);

                auto arguments = new Expressions();
                arguments.reserve(5);
                if (global.params.tracegc)
                {
                    auto funcname = (sc.callsc && sc.callsc.func) ? sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
                    arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
                    arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
                    arguments.push(new StringExp(exp.loc, funcname.toDString()));
                }

                arguments.push(exp.e1);
                arguments.push(exp.e2);
                Expression ce = new CallExp(exp.loc, id, arguments);
                *output = ce.expressionSemantic(sc);
            }
            else if (exp.op == EXP.concatenateElemAssign)
            {
                /* Do not lower concats to the indices array returned by
                 *`static foreach`, as this array is only used at compile-time.
                 */
                if (auto ve = exp.e1.isVarExp)
                {
                    import core.stdc.ctype : isdigit;
                    // The name of the indices array that static foreach loops uses.
                    // See dmd.cond.lowerNonArrayAggregate
                    enum varName = "__res";
                    const(char)[] id = ve.var.ident.toString;
                    if (ve.var.storage_class & STC.temp && id.length > varName.length &&
                        id[0 .. varName.length] == varName && id[varName.length].isdigit)
                        return;
                }

                Identifier hook = global.params.tracegc ? Id._d_arrayappendcTXTrace : Id._d_arrayappendcTX;
                if (!verifyHookExist(exp.loc, *sc, Id._d_arrayappendcTXImpl, "appending element to arrays", Id.object))
                    return setError();

                // Lower to object._d_arrayappendcTXImpl!(typeof(e1))._d_arrayappendcTX{,Trace}(e1, 1), e1[$-1]=e2
                Expression id = new IdentifierExp(exp.loc, Id.empty);
                id = new DotIdExp(exp.loc, id, Id.object);
                auto tiargs = new Objects();
                tiargs.push(exp.e1.type);
                id = new DotTemplateInstanceExp(exp.loc, id, Id._d_arrayappendcTXImpl, tiargs);
                id = new DotIdExp(exp.loc, id, hook);

                auto arguments = new Expressions();
                arguments.reserve(5);
                if (global.params.tracegc)
                {
                    auto funcname = (sc.callsc && sc.callsc.func) ? sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
                    arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
                    arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
                    arguments.push(new StringExp(exp.loc, funcname.toDString()));
                }

                Expression eValue1;
                Expression value1 = extractSideEffect(sc, "__appendtmp", eValue1, exp.e1);

                arguments.push(value1);
                arguments.push(new IntegerExp(exp.loc, 1, Type.tsize_t));

                Expression ce = new CallExp(exp.loc, id, arguments);

                Expression eValue2;
                Expression value2 = exp.e2;
                if (!value2.isVarExp() && !value2.isConst())
                {
                    /* Before the template hook, this check was performed in e2ir.d
                     * for expressions like `a ~= a[$-1]`. Here, $ will be modified
                     * by calling `_d_arrayappendcT`, so we need to save `a[$-1]` in
                     * a temporary variable.
                     */
                    value2 = extractSideEffect(sc, "__appendtmp", eValue2, value2, true);
                    exp.e2 = value2;

                    // `__appendtmp*` will be destroyed together with the array `exp.e1`.
                    auto vd = eValue2.isDeclarationExp().declaration.isVarDeclaration();
                    vd.storage_class |= STC.nodtor;
                    // Be more explicit that this "declaration" is local to the expression
                    vd.storage_class |= STC.exptemp;
                }

                auto ale = new ArrayLengthExp(exp.loc, value1);
                auto elem = new IndexExp(exp.loc, value1, new MinExp(exp.loc, ale, IntegerExp.literal!1));
                auto ae = new ConstructExp(exp.loc, elem, value2);

                auto e0 = Expression.combine(ce, ae).expressionSemantic(sc);
                e0 = Expression.combine(e0, value1);
                e0 = Expression.combine(eValue1, e0);

                e0 = Expression.combine(eValue2, e0);

                *output = e0.expressionSemantic(sc);
            }
        }

    }

    override void visit(AddExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("AddExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        /* ImportC: convert arrays to pointers, functions to pointers to functions
         */
        exp.e1 = exp.e1.arrayFuncConv(sc);
        exp.e2 = exp.e2.arrayFuncConv(sc);

        Type tb1 = exp.e1.type.toBasetype();
        Type tb2 = exp.e2.type.toBasetype();

        bool err = false;
        if (tb1.ty == Tdelegate || tb1.isPtrToFunction())
        {
            err |= exp.e1.checkArithmetic() || exp.e1.checkSharedAccess(sc);
        }
        if (tb2.ty == Tdelegate || tb2.isPtrToFunction())
        {
            err |= exp.e2.checkArithmetic() || exp.e2.checkSharedAccess(sc);
        }
        if (err)
            return setError();

        if (tb1.ty == Tpointer && exp.e2.type.isintegral() || tb2.ty == Tpointer && exp.e1.type.isintegral())
        {
            result = scaleFactor(exp, sc);
            return;
        }

        if (tb1.ty == Tpointer && tb2.ty == Tpointer)
        {
            result = exp.incompatibleTypes();
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }

        tb1 = exp.e1.type.toBasetype();
        if (!target.isVectorOpSupported(tb1, exp.op, tb2))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if ((tb1.isreal() && exp.e2.type.isimaginary()) || (tb1.isimaginary() && exp.e2.type.isreal()))
        {
            switch (exp.type.toBasetype().ty)
            {
            case Tfloat32:
            case Timaginary32:
                exp.type = Type.tcomplex32;
                break;

            case Tfloat64:
            case Timaginary64:
                exp.type = Type.tcomplex64;
                break;

            case Tfloat80:
            case Timaginary80:
                exp.type = Type.tcomplex80;
                break;

            default:
                assert(0);
            }
        }
        result = exp;
    }

    override void visit(MinExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("MinExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        /* ImportC: convert arrays to pointers, functions to pointers to functions
         */
        exp.e1 = exp.e1.arrayFuncConv(sc);
        exp.e2 = exp.e2.arrayFuncConv(sc);

        Type t1 = exp.e1.type.toBasetype();
        Type t2 = exp.e2.type.toBasetype();

        bool err = false;
        if (t1.ty == Tdelegate || t1.isPtrToFunction())
        {
            err |= exp.e1.checkArithmetic() || exp.e1.checkSharedAccess(sc);
        }
        if (t2.ty == Tdelegate || t2.isPtrToFunction())
        {
            err |= exp.e2.checkArithmetic() || exp.e2.checkSharedAccess(sc);
        }
        if (err)
            return setError();

        if (t1.ty == Tpointer)
        {
            if (t2.ty == Tpointer)
            {
                // https://dlang.org/spec/expression.html#add_expressions
                // "If both operands are pointers, and the operator is -, the pointers are
                // subtracted and the result is divided by the size of the type pointed to
                // by the operands. It is an error if the pointers point to different types."
                Type p1 = t1.nextOf();
                Type p2 = t2.nextOf();

                if (!p1.equivalent(p2))
                {
                    // Deprecation to remain for at least a year, after which this should be
                    // changed to an error
                    // See https://github.com/dlang/dmd/pull/7332
                    deprecation(exp.loc,
                        "cannot subtract pointers to different types: `%s` and `%s`.",
                        t1.toChars(), t2.toChars());
                }

                // Need to divide the result by the stride
                // Replace (ptr - ptr) with (ptr - ptr) / stride
                long stride;

                // make sure pointer types are compatible
                if (Expression ex = typeCombine(exp, sc))
                {
                    result = ex;
                    return;
                }

                exp.type = Type.tptrdiff_t;
                stride = t2.nextOf().size();
                if (stride == 0)
                {
                    e = new IntegerExp(exp.loc, 0, Type.tptrdiff_t);
                }
                else if (stride == cast(long)SIZE_INVALID)
                    e = ErrorExp.get();
                else
                {
                    e = new DivExp(exp.loc, exp, new IntegerExp(Loc.initial, stride, Type.tptrdiff_t));
                    e.type = Type.tptrdiff_t;
                }
            }
            else if (t2.isintegral())
                e = scaleFactor(exp, sc);
            else
            {
                exp.error("can't subtract `%s` from pointer", t2.toChars());
                e = ErrorExp.get();
            }
            result = e;
            return;
        }
        if (t2.ty == Tpointer)
        {
            exp.type = exp.e2.type;
            exp.error("can't subtract pointer from `%s`", exp.e1.type.toChars());
            return setError();
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }

        t1 = exp.e1.type.toBasetype();
        t2 = exp.e2.type.toBasetype();
        if (!target.isVectorOpSupported(t1, exp.op, t2))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if ((t1.isreal() && t2.isimaginary()) || (t1.isimaginary() && t2.isreal()))
        {
            switch (exp.type.ty)
            {
            case Tfloat32:
            case Timaginary32:
                exp.type = Type.tcomplex32;
                break;

            case Tfloat64:
            case Timaginary64:
                exp.type = Type.tcomplex64;
                break;

            case Tfloat80:
            case Timaginary80:
                exp.type = Type.tcomplex80;
                break;

            default:
                assert(0);
            }
        }
        result = exp;
        return;
    }

    override void visit(CatExp exp)
    {
        // https://dlang.org/spec/expression.html#cat_expressions
        //printf("CatExp.semantic() %s\n", toChars());
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        Type tb1 = exp.e1.type.toBasetype();
        Type tb2 = exp.e2.type.toBasetype();

        auto f1 = checkNonAssignmentArrayOp(exp.e1);
        auto f2 = checkNonAssignmentArrayOp(exp.e2);
        if (f1 || f2)
            return setError();

        Type tb1next = tb1.nextOf();
        Type tb2next = tb2.nextOf();

        // Check for: array ~ array
        if (tb1next && tb2next && (tb1next.implicitConvTo(tb2next) >= MATCH.constant || tb2next.implicitConvTo(tb1next) >= MATCH.constant || exp.e1.op == EXP.arrayLiteral && exp.e1.implicitConvTo(tb2) || exp.e2.op == EXP.arrayLiteral && exp.e2.implicitConvTo(tb1)))
        {
            /* https://issues.dlang.org/show_bug.cgi?id=9248
             * Here to avoid the case of:
             *    void*[] a = [cast(void*)1];
             *    void*[] b = [cast(void*)2];
             *    a ~ b;
             * becoming:
             *    a ~ [cast(void*)b];
             */

            /* https://issues.dlang.org/show_bug.cgi?id=14682
             * Also to avoid the case of:
             *    int[][] a;
             *    a ~ [];
             * becoming:
             *    a ~ cast(int[])[];
             */
            goto Lpeer;
        }

        // Check for: array ~ element
        if ((tb1.ty == Tsarray || tb1.ty == Tarray) && tb2.ty != Tvoid)
        {
            if (exp.e1.op == EXP.arrayLiteral)
            {
                exp.e2 = doCopyOrMove(sc, exp.e2);
                // https://issues.dlang.org/show_bug.cgi?id=14686
                // Postblit call appears in AST, and this is
                // finally translated  to an ArrayLiteralExp in below optimize().
            }
            else if (exp.e1.op == EXP.string_)
            {
                // No postblit call exists on character (integer) value.
            }
            else
            {
                if (exp.e2.checkPostblit(sc, tb2))
                    return setError();
                // Postblit call will be done in runtime helper function
            }

            if (exp.e1.op == EXP.arrayLiteral && exp.e1.implicitConvTo(tb2.arrayOf()))
            {
                exp.e1 = exp.e1.implicitCastTo(sc, tb2.arrayOf());
                exp.type = tb2.arrayOf();
                goto L2elem;
            }
            if (exp.e2.implicitConvTo(tb1next) >= MATCH.convert)
            {
                exp.e2 = exp.e2.implicitCastTo(sc, tb1next);
                exp.type = tb1next.arrayOf();
            L2elem:
                if (tb2.ty == Tarray || tb2.ty == Tsarray)
                {
                    // Make e2 into [e2]
                    exp.e2 = new ArrayLiteralExp(exp.e2.loc, exp.type, exp.e2);
                }
                else if (checkNewEscape(sc, exp.e2, false))
                    return setError();
                result = exp.optimize(WANTvalue);
                return;
            }
        }
        // Check for: element ~ array
        if ((tb2.ty == Tsarray || tb2.ty == Tarray) && tb1.ty != Tvoid)
        {
            if (exp.e2.op == EXP.arrayLiteral)
            {
                exp.e1 = doCopyOrMove(sc, exp.e1);
            }
            else if (exp.e2.op == EXP.string_)
            {
            }
            else
            {
                if (exp.e1.checkPostblit(sc, tb1))
                    return setError();
            }

            if (exp.e2.op == EXP.arrayLiteral && exp.e2.implicitConvTo(tb1.arrayOf()))
            {
                exp.e2 = exp.e2.implicitCastTo(sc, tb1.arrayOf());
                exp.type = tb1.arrayOf();
                goto L1elem;
            }
            if (exp.e1.implicitConvTo(tb2next) >= MATCH.convert)
            {
                exp.e1 = exp.e1.implicitCastTo(sc, tb2next);
                exp.type = tb2next.arrayOf();
            L1elem:
                if (tb1.ty == Tarray || tb1.ty == Tsarray)
                {
                    // Make e1 into [e1]
                    exp.e1 = new ArrayLiteralExp(exp.e1.loc, exp.type, exp.e1);
                }
                else if (checkNewEscape(sc, exp.e1, false))
                    return setError();
                result = exp.optimize(WANTvalue);
                return;
            }
        }

    Lpeer:
        if ((tb1.ty == Tsarray || tb1.ty == Tarray) && (tb2.ty == Tsarray || tb2.ty == Tarray) && (tb1next.mod || tb2next.mod) && (tb1next.mod != tb2next.mod))
        {
            Type t1 = tb1next.mutableOf().constOf().arrayOf();
            Type t2 = tb2next.mutableOf().constOf().arrayOf();
            if (exp.e1.op == EXP.string_ && !(cast(StringExp)exp.e1).committed)
                exp.e1.type = t1;
            else
                exp.e1 = exp.e1.castTo(sc, t1);
            if (exp.e2.op == EXP.string_ && !(cast(StringExp)exp.e2).committed)
                exp.e2.type = t2;
            else
                exp.e2 = exp.e2.castTo(sc, t2);
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }
        exp.type = exp.type.toHeadMutable();

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tsarray)
            exp.type = tb.nextOf().arrayOf();
        if (exp.type.ty == Tarray && tb1next && tb2next && tb1next.mod != tb2next.mod)
        {
            exp.type = exp.type.nextOf().toHeadMutable().arrayOf();
        }
        if (Type tbn = tb.nextOf())
        {
            if (exp.checkPostblit(sc, tbn))
                return setError();
        }
        Type t1 = exp.e1.type.toBasetype();
        Type t2 = exp.e2.type.toBasetype();
        if ((t1.ty == Tarray || t1.ty == Tsarray) &&
            (t2.ty == Tarray || t2.ty == Tsarray))
        {
            // Normalize to ArrayLiteralExp or StringExp as far as possible
            e = exp.optimize(WANTvalue);
        }
        else
        {
            //printf("(%s) ~ (%s)\n", e1.toChars(), e2.toChars());
            result = exp.incompatibleTypes();
            return;
        }

        result = e;
    }

    override void visit(MulExp exp)
    {
        version (none)
        {
            printf("MulExp::semantic() %s\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }

        if (exp.checkArithmeticBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (exp.type.isfloating())
        {
            Type t1 = exp.e1.type;
            Type t2 = exp.e2.type;

            if (t1.isreal())
            {
                exp.type = t2;
            }
            else if (t2.isreal())
            {
                exp.type = t1;
            }
            else if (t1.isimaginary())
            {
                if (t2.isimaginary())
                {
                    switch (t1.toBasetype().ty)
                    {
                    case Timaginary32:
                        exp.type = Type.tfloat32;
                        break;

                    case Timaginary64:
                        exp.type = Type.tfloat64;
                        break;

                    case Timaginary80:
                        exp.type = Type.tfloat80;
                        break;

                    default:
                        assert(0);
                    }

                    // iy * iv = -yv
                    exp.e1.type = exp.type;
                    exp.e2.type = exp.type;
                    e = new NegExp(exp.loc, exp);
                    e = e.expressionSemantic(sc);
                    result = e;
                    return;
                }
                else
                    exp.type = t2; // t2 is complex
            }
            else if (t2.isimaginary())
            {
                exp.type = t1; // t1 is complex
            }
        }
        else if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        result = exp;
    }

    override void visit(DivExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }

        if (exp.checkArithmeticBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (exp.type.isfloating())
        {
            Type t1 = exp.e1.type;
            Type t2 = exp.e2.type;

            if (t1.isreal())
            {
                exp.type = t2;
                if (t2.isimaginary())
                {
                    // x/iv = i(-x/v)
                    exp.e2.type = t1;
                    e = new NegExp(exp.loc, exp);
                    e = e.expressionSemantic(sc);
                    result = e;
                    return;
                }
            }
            else if (t2.isreal())
            {
                exp.type = t1;
            }
            else if (t1.isimaginary())
            {
                if (t2.isimaginary())
                {
                    switch (t1.toBasetype().ty)
                    {
                    case Timaginary32:
                        exp.type = Type.tfloat32;
                        break;

                    case Timaginary64:
                        exp.type = Type.tfloat64;
                        break;

                    case Timaginary80:
                        exp.type = Type.tfloat80;
                        break;

                    default:
                        assert(0);
                    }
                }
                else
                    exp.type = t2; // t2 is complex
            }
            else if (t2.isimaginary())
            {
                exp.type = t1; // t1 is complex
            }
        }
        else if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        result = exp;
    }

    override void visit(ModExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }
        if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }

        if (exp.checkArithmeticBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (exp.type.isfloating())
        {
            exp.type = exp.e1.type;
            if (exp.e2.type.iscomplex())
            {
                exp.error("cannot perform modulo complex arithmetic");
                return setError();
            }
        }
        result = exp;
    }

    override void visit(PowExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        //printf("PowExp::semantic() %s\n", toChars());
        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }

        if (exp.checkArithmeticBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }

        // First, attempt to fold the expression.
        e = exp.optimize(WANTvalue);
        if (e.op != EXP.pow)
        {
            e = e.expressionSemantic(sc);
            result = e;
            return;
        }

        Module mmath = Module.loadStdMath();
        if (!mmath)
        {
            e.error("`%s` requires `std.math` for `^^` operators", e.toChars());
            return setError();
        }
        e = new ScopeExp(exp.loc, mmath);

        if (exp.e2.op == EXP.float64 && exp.e2.toReal() == CTFloat.half)
        {
            // Replace e1 ^^ 0.5 with .std.math.sqrt(e1)
            e = new CallExp(exp.loc, new DotIdExp(exp.loc, e, Id._sqrt), exp.e1);
        }
        else
        {
            // Replace e1 ^^ e2 with .std.math.pow(e1, e2)
            e = new CallExp(exp.loc, new DotIdExp(exp.loc, e, Id._pow), exp.e1, exp.e2);
        }
        e = e.expressionSemantic(sc);
        result = e;
        return;
    }

    override void visit(ShlExp exp)
    {
        //printf("ShlExp::semantic(), type = %p\n", type);
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (!target.isVectorOpSupported(exp.e1.type.toBasetype(), exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        exp.e1 = integralPromotions(exp.e1, sc);
        if (exp.e2.type.toBasetype().ty != Tvector)
            exp.e2 = exp.e2.castTo(sc, Type.tshiftcnt);

        exp.type = exp.e1.type;
        result = exp;
    }

    override void visit(ShrExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (!target.isVectorOpSupported(exp.e1.type.toBasetype(), exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        exp.e1 = integralPromotions(exp.e1, sc);
        if (exp.e2.type.toBasetype().ty != Tvector)
            exp.e2 = exp.e2.castTo(sc, Type.tshiftcnt);

        exp.type = exp.e1.type;
        result = exp;
    }

    override void visit(UshrExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc))
            return setError();

        if (!target.isVectorOpSupported(exp.e1.type.toBasetype(), exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        exp.e1 = integralPromotions(exp.e1, sc);
        if (exp.e2.type.toBasetype().ty != Tvector)
            exp.e2 = exp.e2.castTo(sc, Type.tshiftcnt);

        exp.type = exp.e1.type;
        result = exp;
    }

    override void visit(AndExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.e1.type.toBasetype().ty == Tbool && exp.e2.type.toBasetype().ty == Tbool)
        {
            exp.type = exp.e1.type;
            result = exp;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }
        if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc))
            return setError();

        result = exp;
    }

    override void visit(OrExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.e1.type.toBasetype().ty == Tbool && exp.e2.type.toBasetype().ty == Tbool)
        {
            exp.type = exp.e1.type;
            result = exp;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }
        if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc))
            return setError();

        result = exp;
    }

    override void visit(XorExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        if (exp.e1.type.toBasetype().ty == Tbool && exp.e2.type.toBasetype().ty == Tbool)
        {
            exp.type = exp.e1.type;
            result = exp;
            return;
        }

        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            if (!isArrayOpValid(exp))
            {
                result = arrayOpInvalidError(exp);
                return;
            }
            result = exp;
            return;
        }
        if (!target.isVectorOpSupported(tb, exp.op, exp.e2.type.toBasetype()))
        {
            result = exp.incompatibleTypes();
            return;
        }
        if (exp.checkIntegralBin() || exp.checkSharedAccessBin(sc))
            return setError();

        result = exp;
    }

    override void visit(LogicalExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("LogicalExp::semantic() %s\n", exp.toChars());
        }

        if (exp.type)
        {
            result = exp;
            return;
        }

        exp.setNoderefOperands();

        Expression e1x = exp.e1.expressionSemantic(sc);

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (e1x.op == EXP.type)
            e1x = resolveAliasThis(sc, e1x);

        e1x = resolveProperties(sc, e1x);
        e1x = e1x.toBoolean(sc);

        if (sc.flags & SCOPE.condition)
        {
            /* If in static if, don't evaluate e2 if we don't have to.
             */
            e1x = e1x.optimize(WANTvalue);
            if (e1x.toBool().hasValue(exp.op == EXP.orOr))
            {
                if (sc.flags & SCOPE.Cfile)
                    result = new IntegerExp(exp.op == EXP.orOr);
                else
                    result = IntegerExp.createBool(exp.op == EXP.orOr);
                return;
            }
        }

        CtorFlow ctorflow = sc.ctorflow.clone();
        Expression e2x = exp.e2.expressionSemantic(sc);
        sc.merge(exp.loc, ctorflow);
        ctorflow.freeFieldinit();

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (e2x.op == EXP.type)
            e2x = resolveAliasThis(sc, e2x);

        e2x = resolveProperties(sc, e2x);

        auto f1 = checkNonAssignmentArrayOp(e1x);
        auto f2 = checkNonAssignmentArrayOp(e2x);
        if (f1 || f2)
            return setError();

        // Unless the right operand is 'void', the expression is converted to 'bool'.
        if (e2x.type.ty != Tvoid)
            e2x = e2x.toBoolean(sc);

        if (e2x.op == EXP.type || e2x.op == EXP.scope_)
        {
            exp.error("`%s` is not an expression", exp.e2.toChars());
            return setError();
        }
        if (e1x.op == EXP.error || e1x.type.ty == Tnoreturn)
        {
            result = e1x;
            return;
        }
        if (e2x.op == EXP.error)
        {
            result = e2x;
            return;
        }

        // The result type is 'bool', unless the right operand has type 'void'.
        if (e2x.type.ty == Tvoid)
            exp.type = Type.tvoid;
        else
            exp.type = (sc && sc.flags & SCOPE.Cfile) ? Type.tint32 : Type.tbool;

        exp.e1 = e1x;
        exp.e2 = e2x;
        result = exp;
    }


    override void visit(CmpExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("CmpExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        exp.setNoderefOperands();

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Type t1 = exp.e1.type.toBasetype();
        Type t2 = exp.e2.type.toBasetype();
        if (t1.ty == Tclass && exp.e2.op == EXP.null_ || t2.ty == Tclass && exp.e1.op == EXP.null_)
        {
            exp.error("do not use `null` when comparing class types");
            return setError();
        }


        EXP cmpop = exp.op;
        if (auto e = exp.op_overload(sc, &cmpop))
        {
            if (!e.type.isscalar() && e.type.equals(exp.e1.type))
            {
                exp.error("recursive `opCmp` expansion");
                return setError();
            }
            if (e.op == EXP.call)
            {

                if (t1.ty == Tclass && t2.ty == Tclass)
                {
                    // Lower to object.__cmp(e1, e2)
                    Expression cl = new IdentifierExp(exp.loc, Id.empty);
                    cl = new DotIdExp(exp.loc, cl, Id.object);
                    cl = new DotIdExp(exp.loc, cl, Id.__cmp);
                    cl = cl.expressionSemantic(sc);

                    auto arguments = new Expressions();
                    // Check if op_overload found a better match by calling e2.opCmp(e1)
                    // If the operands were swapped, then the result must be reversed
                    // e1.opCmp(e2) == -e2.opCmp(e1)
                    // cmpop takes care of this
                    if (exp.op == cmpop)
                    {
                        arguments.push(exp.e1);
                        arguments.push(exp.e2);
                    }
                    else
                    {
                        // Use better match found by op_overload
                        arguments.push(exp.e2);
                        arguments.push(exp.e1);
                    }

                    cl = new CallExp(exp.loc, cl, arguments);
                    cl = new CmpExp(cmpop, exp.loc, cl, new IntegerExp(0));
                    result = cl.expressionSemantic(sc);
                    return;
                }

                e = new CmpExp(cmpop, exp.loc, e, IntegerExp.literal!0);
                e = e.expressionSemantic(sc);
            }
            result = e;
            return;
        }


        if (Expression ex = typeCombine(exp, sc))
        {
            result = ex;
            return;
        }

        auto f1 = checkNonAssignmentArrayOp(exp.e1);
        auto f2 = checkNonAssignmentArrayOp(exp.e2);
        if (f1 || f2)
            return setError();

        exp.type = (sc && sc.flags & SCOPE.Cfile) ? Type.tint32 : Type.tbool;

        // Special handling for array comparisons
        Expression arrayLowering = null;
        t1 = exp.e1.type.toBasetype();
        t2 = exp.e2.type.toBasetype();
        if ((t1.ty == Tarray || t1.ty == Tsarray || t1.ty == Tpointer) && (t2.ty == Tarray || t2.ty == Tsarray || t2.ty == Tpointer))
        {
            Type t1next = t1.nextOf();
            Type t2next = t2.nextOf();
            if (t1next.implicitConvTo(t2next) < MATCH.constant && t2next.implicitConvTo(t1next) < MATCH.constant && (t1next.ty != Tvoid && t2next.ty != Tvoid))
            {
                exp.error("array comparison type mismatch, `%s` vs `%s`", t1next.toChars(), t2next.toChars());
                return setError();
            }
            if ((t1.ty == Tarray || t1.ty == Tsarray) && (t2.ty == Tarray || t2.ty == Tsarray))
            {
                if (!verifyHookExist(exp.loc, *sc, Id.__cmp, "comparing arrays"))
                    return setError();

                // Lower to object.__cmp(e1, e2)
                Expression al = new IdentifierExp(exp.loc, Id.empty);
                al = new DotIdExp(exp.loc, al, Id.object);
                al = new DotIdExp(exp.loc, al, Id.__cmp);
                al = al.expressionSemantic(sc);

                auto arguments = new Expressions(2);
                (*arguments)[0] = exp.e1;
                (*arguments)[1] = exp.e2;

                al = new CallExp(exp.loc, al, arguments);
                al = new CmpExp(exp.op, exp.loc, al, IntegerExp.literal!0);

                arrayLowering = al;
            }
        }
        else if (t1.ty == Tstruct || t2.ty == Tstruct || (t1.ty == Tclass && t2.ty == Tclass))
        {
            if (t2.ty == Tstruct)
                exp.error("need member function `opCmp()` for %s `%s` to compare", t2.toDsymbol(sc).kind(), t2.toChars());
            else
                exp.error("need member function `opCmp()` for %s `%s` to compare", t1.toDsymbol(sc).kind(), t1.toChars());
            return setError();
        }
        else if (t1.iscomplex() || t2.iscomplex())
        {
            exp.error("compare not defined for complex operands");
            return setError();
        }
        else if (t1.ty == Taarray || t2.ty == Taarray)
        {
            exp.error("`%s` is not defined for associative arrays", EXPtoString(exp.op).ptr);
            return setError();
        }
        else if (!target.isVectorOpSupported(t1, exp.op, t2))
        {
            result = exp.incompatibleTypes();
            return;
        }
        else
        {
            bool r1 = exp.e1.checkValue() || exp.e1.checkSharedAccess(sc);
            bool r2 = exp.e2.checkValue() || exp.e2.checkSharedAccess(sc);
            if (r1 || r2)
                return setError();
        }

        //printf("CmpExp: %s, type = %s\n", e.toChars(), e.type.toChars());
        if (arrayLowering)
        {
            arrayLowering = arrayLowering.expressionSemantic(sc);
            result = arrayLowering;
            return;
        }

        if (auto tv = t1.isTypeVector())
            exp.type = tv.toBooleanVector();

        result = exp;
        return;
    }

    override void visit(InExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (Expression ex = binSemanticProp(exp, sc))
        {
            result = ex;
            return;
        }
        Expression e = exp.op_overload(sc);
        if (e)
        {
            result = e;
            return;
        }

        Type t2b = exp.e2.type.toBasetype();
        switch (t2b.ty)
        {
        case Taarray:
            {
                TypeAArray ta = cast(TypeAArray)t2b;

                // Special handling for array keys
                if (!arrayTypeCompatibleWithoutCasting(exp.e1.type, ta.index))
                {
                    // Convert key to type of key
                    exp.e1 = exp.e1.implicitCastTo(sc, ta.index);
                }

                semanticTypeInfo(sc, ta.index);

                // Return type is pointer to value
                exp.type = ta.nextOf().pointerTo();
                break;
            }

        case Terror:
            return setError();

        case Tarray, Tsarray:
            result = exp.incompatibleTypes();
            exp.errorSupplemental("`in` is only allowed on associative arrays");
            const(char)* slice = (t2b.ty == Tsarray) ? "[]" : "";
            exp.errorSupplemental("perhaps use `std.algorithm.find(%s, %s%s)` instead",
                exp.e1.toChars(), exp.e2.toChars(), slice);
            return;

        default:
            result = exp.incompatibleTypes();
            return;
        }
        result = exp;
    }

    override void visit(RemoveExp e)
    {
        if (Expression ex = binSemantic(e, sc))
        {
            result = ex;
            return;
        }
        result = e;
    }

    override void visit(EqualExp exp)
    {
        //printf("EqualExp::semantic('%s')\n", exp.toChars());
        if (exp.type)
        {
            result = exp;
            return;
        }

        exp.setNoderefOperands();

        if (auto e = binSemanticProp(exp, sc))
        {
            result = e;
            return;
        }
        if (exp.e1.op == EXP.type || exp.e2.op == EXP.type)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=12520
             * empty tuples are represented as types so special cases are added
             * so that they can be compared for equality with tuples of values.
             */
            static auto extractTypeTupAndExpTup(Expression e)
            {
                static struct Result { bool ttEmpty; bool te; }
                auto tt = e.op == EXP.type ? e.isTypeExp().type.isTypeTuple() : null;
                return Result(tt && (!tt.arguments || !tt.arguments.length), e.isTupleExp() !is null);
            }
            auto tups1 = extractTypeTupAndExpTup(exp.e1);
            auto tups2 = extractTypeTupAndExpTup(exp.e2);
            // AliasSeq!() == AliasSeq!(<at least a value>)
            if (tups1.ttEmpty && tups2.te)
            {
                result = IntegerExp.createBool(exp.op != EXP.equal);
                return;
            }
            // AliasSeq!(<at least a value>) == AliasSeq!()
            else if (tups1.te && tups2.ttEmpty)
            {
                result = IntegerExp.createBool(exp.op != EXP.equal);
                return;
            }
            // AliasSeq!() == AliasSeq!()
            else if (tups1.ttEmpty && tups2.ttEmpty)
            {
                result = IntegerExp.createBool(exp.op == EXP.equal);
                return;
            }
            // otherwise, two types are really not comparable
            result = exp.incompatibleTypes();
            return;
        }

        {
            auto t1 = exp.e1.type;
            auto t2 = exp.e2.type;
            if (t1.ty == Tenum && t2.ty == Tenum && !t1.equivalent(t2))
                exp.error("comparison between different enumeration types `%s` and `%s`; If this behavior is intended consider using `std.conv.asOriginalType`",
                    t1.toChars(), t2.toChars());
        }

        /* Before checking for operator overloading, check to see if we're
         * comparing the addresses of two statics. If so, we can just see
         * if they are the same symbol.
         */
        if (exp.e1.op == EXP.address && exp.e2.op == EXP.address)
        {
            AddrExp ae1 = cast(AddrExp)exp.e1;
            AddrExp ae2 = cast(AddrExp)exp.e2;
            if (ae1.e1.op == EXP.variable && ae2.e1.op == EXP.variable)
            {
                VarExp ve1 = cast(VarExp)ae1.e1;
                VarExp ve2 = cast(VarExp)ae2.e1;
                if (ve1.var == ve2.var)
                {
                    // They are the same, result is 'true' for ==, 'false' for !=
                    result = IntegerExp.createBool(exp.op == EXP.equal);
                    return;
                }
            }
        }

        Type t1 = exp.e1.type.toBasetype();
        Type t2 = exp.e2.type.toBasetype();

        // Indicates whether the comparison of the 2 specified array types
        // requires an object.__equals() lowering.
        static bool needsDirectEq(Type t1, Type t2, Scope* sc)
        {
            Type t1n = t1.nextOf().toBasetype();
            Type t2n = t2.nextOf().toBasetype();
            if ((t1n.ty.isSomeChar && t2n.ty.isSomeChar) ||
                (t1n.ty == Tvoid || t2n.ty == Tvoid))
            {
                return false;
            }
            if (t1n.constOf() != t2n.constOf())
                return true;

            Type t = t1n;
            while (t.toBasetype().nextOf())
                t = t.nextOf().toBasetype();
            if (auto ts = t.isTypeStruct())
            {
                // semanticTypeInfo() makes sure hasIdentityEquals has been computed
                if (global.params.useTypeInfo && Type.dtypeinfo)
                    semanticTypeInfo(sc, ts);

                return ts.sym.hasIdentityEquals; // has custom opEquals
            }

            return false;
        }

        if (auto e = exp.op_overload(sc))
        {
            result = e;
            return;
        }


        const isArrayComparison = (t1.ty == Tarray || t1.ty == Tsarray) &&
                                  (t2.ty == Tarray || t2.ty == Tsarray);
        const needsArrayLowering = isArrayComparison && needsDirectEq(t1, t2, sc);

        if (!needsArrayLowering)
        {
            // https://issues.dlang.org/show_bug.cgi?id=23783
            if (exp.e1.checkSharedAccess(sc) || exp.e2.checkSharedAccess(sc))
                return setError();
            if (auto e = typeCombine(exp, sc))
            {
                result = e;
                return;
            }
        }

        auto f1 = checkNonAssignmentArrayOp(exp.e1);
        auto f2 = checkNonAssignmentArrayOp(exp.e2);
        if (f1 || f2)
            return setError();

        exp.type = (sc && sc.flags & SCOPE.Cfile) ? Type.tint32 : Type.tbool;

        if (!isArrayComparison)
        {
            if (exp.e1.type != exp.e2.type && exp.e1.type.isfloating() && exp.e2.type.isfloating())
            {
                // Cast both to complex
                exp.e1 = exp.e1.castTo(sc, Type.tcomplex80);
                exp.e2 = exp.e2.castTo(sc, Type.tcomplex80);
            }
        }

        // lower some array comparisons to object.__equals(e1, e2)
        if (needsArrayLowering || (t1.ty == Tarray && t2.ty == Tarray))
        {
            //printf("Lowering to __equals %s %s\n", exp.e1.toChars(), exp.e2.toChars());

            // https://issues.dlang.org/show_bug.cgi?id=22390
            // Equality comparison between array of noreturns simply lowers to length equality comparison
            if (t1.nextOf.isTypeNoreturn() && t2.nextOf.isTypeNoreturn())
            {
                Expression exp_l1 = new DotIdExp(exp.e1.loc, exp.e1, Id.length);
                Expression exp_l2 = new DotIdExp(exp.e2.loc, exp.e2, Id.length);
                auto e = new EqualExp(EXP.equal, exp.loc, exp_l1, exp_l2);
                result = e.expressionSemantic(sc);
                return;
            }

            if (!verifyHookExist(exp.loc, *sc, Id.__equals, "equal checks on arrays"))
                return setError();

            Expression __equals = new IdentifierExp(exp.loc, Id.empty);
            Identifier id = Identifier.idPool("__equals");
            __equals = new DotIdExp(exp.loc, __equals, Id.object);
            __equals = new DotIdExp(exp.loc, __equals, id);

            /* https://issues.dlang.org/show_bug.cgi?id=23674
             *
             * Optimize before creating the call expression to the
             * druntime hook as the optimizer may output errors
             * that will get swallowed otherwise.
             */
            exp.e1 = exp.e1.optimize(WANTvalue);
            exp.e2 = exp.e2.optimize(WANTvalue);

            auto arguments = new Expressions(2);
            (*arguments)[0] = exp.e1;
            (*arguments)[1] = exp.e2;

            __equals = new CallExp(exp.loc, __equals, arguments);
            if (exp.op == EXP.notEqual)
            {
                __equals = new NotExp(exp.loc, __equals);
            }
            __equals = __equals.trySemantic(sc); // for better error message
            if (!__equals)
            {
                exp.error("incompatible types for array comparison: `%s` and `%s`",
                          exp.e1.type.toChars(), exp.e2.type.toChars());
                __equals = ErrorExp.get();
            }

            result = __equals;
            return;
        }

        if (exp.e1.type.toBasetype().ty == Taarray)
            semanticTypeInfo(sc, exp.e1.type.toBasetype());


        if (!target.isVectorOpSupported(t1, exp.op, t2))
        {
            result = exp.incompatibleTypes();
            return;
        }

        if (auto tv = t1.isTypeVector())
            exp.type = tv.toBooleanVector();

        result = exp;
    }

    override void visit(IdentityExp exp)
    {
        if (exp.type)
        {
            result = exp;
            return;
        }

        exp.setNoderefOperands();

        if (auto e = binSemanticProp(exp, sc))
        {
            result = e;
            return;
        }

        if (auto e = typeCombine(exp, sc))
        {
            result = e;
            return;
        }

        auto f1 = checkNonAssignmentArrayOp(exp.e1);
        auto f2 = checkNonAssignmentArrayOp(exp.e2);
        if (f1 || f2)
            return setError();

        if (exp.e1.op == EXP.type || exp.e2.op == EXP.type)
        {
            result = exp.incompatibleTypes();
            return;
        }

        exp.type = Type.tbool;

        if (exp.e1.type != exp.e2.type && exp.e1.type.isfloating() && exp.e2.type.isfloating())
        {
            // Cast both to complex
            exp.e1 = exp.e1.castTo(sc, Type.tcomplex80);
            exp.e2 = exp.e2.castTo(sc, Type.tcomplex80);
        }

        auto tb1 = exp.e1.type.toBasetype();
        auto tb2 = exp.e2.type.toBasetype();
        if (!target.isVectorOpSupported(tb1, exp.op, tb2))
        {
            result = exp.incompatibleTypes();
            return;
        }

        if (exp.e1.op == EXP.call)
            exp.e1 = (cast(CallExp)exp.e1).addDtorHook(sc);
        if (exp.e2.op == EXP.call)
            exp.e2 = (cast(CallExp)exp.e2).addDtorHook(sc);

        if (exp.e1.type.toBasetype().ty == Tsarray ||
            exp.e2.type.toBasetype().ty == Tsarray)
            exp.deprecation("identity comparison of static arrays "
                ~ "implicitly coerces them to slices, "
                ~ "which are compared by reference");

        result = exp;
    }

    override void visit(CondExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("CondExp::semantic('%s')\n", exp.toChars());
        }
        if (exp.type)
        {
            result = exp;
            return;
        }

        if (auto die = exp.econd.isDotIdExp())
            die.noderef = true;

        Expression ec = exp.econd.expressionSemantic(sc);
        ec = resolveProperties(sc, ec);
        ec = ec.toBoolean(sc);

        CtorFlow ctorflow_root = sc.ctorflow.clone();
        Expression e1x = exp.e1.expressionSemantic(sc).arrayFuncConv(sc);
        e1x = resolveProperties(sc, e1x);

        CtorFlow ctorflow1 = sc.ctorflow;
        sc.ctorflow = ctorflow_root;
        Expression e2x = exp.e2.expressionSemantic(sc).arrayFuncConv(sc);
        e2x = resolveProperties(sc, e2x);

        sc.merge(exp.loc, ctorflow1);
        ctorflow1.freeFieldinit();

        if (ec.op == EXP.error)
        {
            result = ec;
            return;
        }
        if (ec.type == Type.terror)
            return setError();
        exp.econd = ec;

        if (e1x.op == EXP.error)
        {
            result = e1x;
            return;
        }
        if (e1x.type == Type.terror)
            return setError();
        exp.e1 = e1x;

        if (e2x.op == EXP.error)
        {
            result = e2x;
            return;
        }
        if (e2x.type == Type.terror)
            return setError();
        exp.e2 = e2x;

        auto f0 = checkNonAssignmentArrayOp(exp.econd);
        auto f1 = checkNonAssignmentArrayOp(exp.e1);
        auto f2 = checkNonAssignmentArrayOp(exp.e2);
        if (f0 || f1 || f2)
            return setError();

        Type t1 = exp.e1.type;
        Type t2 = exp.e2.type;

        // https://issues.dlang.org/show_bug.cgi?id=23767
        // `cast(void*) 0` should be treated as `null` so the ternary expression
        // gets the pointer type of the other branch
        if (sc.flags & SCOPE.Cfile)
        {
            static void rewriteCNull(ref Expression e, ref Type t)
            {
                if (!t.isTypePointer())
                    return;
                if (auto ie = e.optimize(WANTvalue).isIntegerExp())
                {
                    if (ie.getInteger() == 0)
                    {
                        e = new NullExp(e.loc, Type.tnull);
                        t = Type.tnull;
                    }
                }
            }
            rewriteCNull(exp.e1, t1);
            rewriteCNull(exp.e2, t2);
        }

        if (t1.ty == Tnoreturn)
        {
            exp.type = t2;
            exp.e1 = specialNoreturnCast(exp.e1, exp.type);
        }
        else if (t2.ty == Tnoreturn)
        {
            exp.type = t1;
            exp.e2 = specialNoreturnCast(exp.e2, exp.type);
        }
        // If either operand is void the result is void, we have to cast both
        // the expression to void so that we explicitly discard the expression
        // value if any
        // https://issues.dlang.org/show_bug.cgi?id=16598
        else if (t1.ty == Tvoid || t2.ty == Tvoid)
        {
            exp.type = Type.tvoid;
            exp.e1 = exp.e1.castTo(sc, exp.type);
            exp.e2 = exp.e2.castTo(sc, exp.type);
        }
        else if (t1 == t2)
            exp.type = t1;
        else
        {
            if (Expression ex = typeCombine(exp, sc))
            {
                result = ex;
                return;
            }

            switch (exp.e1.type.toBasetype().ty)
            {
            case Tcomplex32:
            case Tcomplex64:
            case Tcomplex80:
                exp.e2 = exp.e2.castTo(sc, exp.e1.type);
                break;
            default:
                break;
            }
            switch (exp.e2.type.toBasetype().ty)
            {
            case Tcomplex32:
            case Tcomplex64:
            case Tcomplex80:
                exp.e1 = exp.e1.castTo(sc, exp.e2.type);
                break;
            default:
                break;
            }
            if (exp.type.toBasetype().ty == Tarray)
            {
                exp.e1 = exp.e1.castTo(sc, exp.type);
                exp.e2 = exp.e2.castTo(sc, exp.type);
            }
        }
        exp.type = exp.type.merge2();
        version (none)
        {
            printf("res: %s\n", exp.type.toChars());
            printf("e1 : %s\n", exp.e1.type.toChars());
            printf("e2 : %s\n", exp.e2.type.toChars());
        }

        /* https://issues.dlang.org/show_bug.cgi?id=14696
         * If either e1 or e2 contain temporaries which need dtor,
         * make them conditional.
         * Rewrite:
         *      cond ? (__tmp1 = ..., __tmp1) : (__tmp2 = ..., __tmp2)
         * to:
         *      (auto __cond = cond) ? (... __tmp1) : (... __tmp2)
         * and replace edtors of __tmp1 and __tmp2 with:
         *      __tmp1.edtor --> __cond && __tmp1.dtor()
         *      __tmp2.edtor --> __cond || __tmp2.dtor()
         */
        exp.hookDtors(sc);

        result = exp;
    }

    override void visit(GenericExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("GenericExp::semantic('%s')\n", exp.toChars());
        }
        // C11 6.5.1.1 Generic Selection

        auto ec = exp.cntlExp.expressionSemantic(sc).arrayFuncConv(sc);
        bool errors = ec.isErrorExp() !is null;
        auto tc = ec.type;

        auto types = (*exp.types)[];
        foreach (i, ref t; types)
        {
            if (!t)
                continue;       // `default:` case
            t = t.typeSemantic(ec.loc, sc);
            if (t.isTypeError())
            {
                errors = true;
                continue;
            }

            /* C11 6.5.1-2 duplicate check
             */
            /* C11 distinguishes int, long, and long long. But D doesn't, so depending on the
             * C target, a long may have the same type as `int` in the D type system.
             * So, skip checks when this may be the case. Later pick the first match
             */
            if (
                (t.ty == Tint32 || t.ty == Tuns32) && target.c.longsize == 4 ||
                (t.ty == Tint64 || t.ty == Tuns64) && target.c.longsize == 8 ||
                (t.ty == Tfloat64 || t.ty == Timaginary64 || t.ty == Tcomplex64) && target.c.long_doublesize == 8
               )
                continue;

            foreach (t2; types[0 .. i])
            {
                if (t2 && t2.equals(t))
                {
                    error(ec.loc, "generic association type `%s` can only appear once", t.toChars());
                    errors = true;
                    break;
                }
            }
        }

        auto exps = (*exp.exps)[];
        foreach (ref e; exps)
        {
            e = e.expressionSemantic(sc);
            if (e.isErrorExp())
                errors = true;
        }

        if (errors)
            return setError();

        enum size_t None = ~0;
        size_t imatch = None;
        size_t idefault = None;
        foreach (const i, t; types)
        {
            if (t)
            {
                /* if tc is compatible with t, it's a match
                 * C11 6.2.7 defines a compatible type as being the same type, including qualifiers
                 */
                if (tc.equals(t))
                {
                    assert(imatch == None);
                    imatch = i;
                    break;              // pick first match
                }
            }
            else
                idefault = i;  // multiple defaults are not allowed, and are caught by cparse
        }

        if (imatch == None)
            imatch = idefault;
        if (imatch == None)
        {
            error(exp.loc, "no compatible generic association type for controlling expression type `%s`", tc.toChars());
            return setError();
        }

        result = exps[imatch];
    }

    override void visit(FileInitExp e)
    {
        //printf("FileInitExp::semantic()\n");
        e.type = Type.tstring;
        result = e;
    }

    override void visit(LineInitExp e)
    {
        e.type = Type.tint32;
        result = e;
    }

    override void visit(ModuleInitExp e)
    {
        //printf("ModuleInitExp::semantic()\n");
        e.type = Type.tstring;
        result = e;
    }

    override void visit(FuncInitExp e)
    {
        //printf("FuncInitExp::semantic()\n");
        e.type = Type.tstring;
        if (sc.func)
        {
            result = e.resolveLoc(Loc.initial, sc);
            return;
        }
        result = e;
    }

    override void visit(PrettyFuncInitExp e)
    {
        //printf("PrettyFuncInitExp::semantic()\n");
        e.type = Type.tstring;
        if (sc.func)
        {
            result = e.resolveLoc(Loc.initial, sc);
            return;
        }

        result = e;
    }
}

/**********************************
 * Try to run semantic routines.
 * If they fail, return NULL.
 */
Expression trySemantic(Expression exp, Scope* sc)
{
    //printf("+trySemantic(%s)\n", exp.toChars());
    uint errors = global.startGagging();
    Expression e = expressionSemantic(exp, sc);
    if (global.endGagging(errors))
    {
        e = null;
    }
    //printf("-trySemantic(%s)\n", exp.toChars());
    return e;
}

/**************************
 * Helper function for easy error propagation.
 * If error occurs, returns ErrorExp. Otherwise returns NULL.
 */
Expression unaSemantic(UnaExp e, Scope* sc)
{
    static if (LOGSEMANTIC)
    {
        printf("UnaExp::semantic('%s')\n", e.toChars());
    }
    Expression e1x = e.e1.expressionSemantic(sc);
    if (e1x.op == EXP.error)
        return e1x;
    e.e1 = e1x;
    return null;
}

/**************************
 * Helper function for easy error propagation.
 * If error occurs, returns ErrorExp. Otherwise returns NULL.
 */
Expression binSemantic(BinExp e, Scope* sc)
{
    static if (LOGSEMANTIC)
    {
        printf("BinExp::semantic('%s')\n", e.toChars());
    }
    Expression e1x = e.e1.expressionSemantic(sc);
    Expression e2x = e.e2.expressionSemantic(sc);

    // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
    if (e1x.op == EXP.type)
        e1x = resolveAliasThis(sc, e1x);
    if (e2x.op == EXP.type)
        e2x = resolveAliasThis(sc, e2x);

    if (e1x.op == EXP.error)
        return e1x;
    if (e2x.op == EXP.error)
        return e2x;
    e.e1 = e1x;
    e.e2 = e2x;
    return null;
}

Expression binSemanticProp(BinExp e, Scope* sc)
{
    if (Expression ex = binSemantic(e, sc))
        return ex;
    Expression e1x = resolveProperties(sc, e.e1);
    Expression e2x = resolveProperties(sc, e.e2);
    if (e1x.op == EXP.error)
        return e1x;
    if (e2x.op == EXP.error)
        return e2x;
    e.e1 = e1x;
    e.e2 = e2x;
    return null;
}

// entrypoint for semantic ExpressionSemanticVisitor
extern (C++) Expression expressionSemantic(Expression e, Scope* sc)
{
    scope v = new ExpressionSemanticVisitor(sc);
    e.accept(v);
    return v.result;
}

private Expression dotIdSemanticPropX(DotIdExp exp, Scope* sc)
{
    //printf("DotIdExp::semanticX(this = %p, '%s')\n", this, toChars());
    if (Expression ex = unaSemantic(exp, sc))
        return ex;

    if (!(sc.flags & SCOPE.Cfile) && exp.ident == Id._mangleof)
    {
        // symbol.mangleof

        // return mangleof as an Expression
        static Expression dotMangleof(const ref Loc loc, Scope* sc, Dsymbol ds)
        {
            assert(ds);
            if (auto f = ds.isFuncDeclaration())
            {
                if (f.checkForwardRef(loc))
                    return ErrorExp.get();

                if (f.purityInprocess || f.safetyInprocess || f.nothrowInprocess || f.nogcInprocess)
                {
                    f.error(loc, "cannot retrieve its `.mangleof` while inferring attributes");
                    return ErrorExp.get();
                }
            }
            OutBuffer buf;
            mangleToBuffer(ds, &buf);
            Expression e = new StringExp(loc, buf.extractSlice());
            return e.expressionSemantic(sc);
        }

        Dsymbol ds;
        switch (exp.e1.op)
        {
            case EXP.scope_:      return dotMangleof(exp.loc, sc, exp.e1.isScopeExp().sds);
            case EXP.variable:    return dotMangleof(exp.loc, sc, exp.e1.isVarExp().var);
            case EXP.dotVariable: return dotMangleof(exp.loc, sc, exp.e1.isDotVarExp().var);
            case EXP.overloadSet: return dotMangleof(exp.loc, sc, exp.e1.isOverExp().vars);
            case EXP.template_:
            {
                TemplateExp te = exp.e1.isTemplateExp();
                return dotMangleof(exp.loc, sc, ds = te.fd ? te.fd.isDsymbol() : te.td);
            }

            default:
                break;
        }
    }

    if (exp.e1.isVarExp() && exp.e1.type.toBasetype().isTypeSArray() && exp.ident == Id.length)
    {
        // bypass checkPurity
        return exp.e1.type.dotExp(sc, exp.e1, exp.ident, exp.noderef ? DotExpFlag.noDeref : 0);
    }

    if (!exp.e1.isDotExp())
    {
        exp.e1 = resolvePropertiesX(sc, exp.e1);
    }

    if (auto te = exp.e1.isTupleExp())
    {
        if (exp.ident == Id.offsetof)
        {
            /* 'distribute' the .offsetof to each of the tuple elements.
             */
            auto exps = new Expressions(te.exps.length);
            foreach (i, e; (*te.exps)[])
            {
                (*exps)[i] = new DotIdExp(e.loc, e, Id.offsetof);
            }
            // Don't evaluate te.e0 in runtime
            Expression e = new TupleExp(exp.loc, null, exps);
            e = e.expressionSemantic(sc);
            return e;
        }
        if (exp.ident == Id.length)
        {
            // Don't evaluate te.e0 in runtime
            return new IntegerExp(exp.loc, te.exps.length, Type.tsize_t);
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=14416
    // Template has no built-in properties except for 'stringof'.
    if ((exp.e1.isDotTemplateExp() || exp.e1.isTemplateExp()) && exp.ident != Id.stringof)
    {
        exp.error("template `%s` does not have property `%s`", exp.e1.toChars(), exp.ident.toChars());
        return ErrorExp.get();
    }
    if (!exp.e1.type)
    {
        exp.error("expression `%s` does not have property `%s`", exp.e1.toChars(), exp.ident.toChars());
        return ErrorExp.get();
    }

    return exp;
}

/******************************
 * Resolve properties, i.e. `e1.ident`, without seeing UFCS.
 * Params:
 *      exp = expression to resolve
 *      sc = context
 *      flag = if 1 then do not emit error messages, just return null
 * Returns:
 *      resolved expression, null if error
 */
Expression dotIdSemanticProp(DotIdExp exp, Scope* sc, int flag)
{
    //printf("DotIdExp::semanticY(this = %p, '%s')\n", exp, exp.toChars());

    //{ static int z; fflush(stdout); if (++z == 10) *(char*)0=0; }

    const cfile = (sc.flags & SCOPE.Cfile) != 0;

    /* Special case: rewrite this.id and super.id
     * to be classtype.id and baseclasstype.id
     * if we have no this pointer.
     */
    if ((exp.e1.isThisExp() || exp.e1.isSuperExp()) && !hasThis(sc))
    {
        if (AggregateDeclaration ad = sc.getStructClassScope())
        {
            if (exp.e1.isThisExp())
            {
                exp.e1 = new TypeExp(exp.e1.loc, ad.type);
            }
            else
            {
                if (auto cd = ad.isClassDeclaration())
                {
                    if (cd.baseClass)
                        exp.e1 = new TypeExp(exp.e1.loc, cd.baseClass.type);
                }
            }
        }
    }

    {
        Expression e = dotIdSemanticPropX(exp, sc);
        if (e != exp)
            return e;
    }

    Expression eleft;
    Expression eright;
    if (auto de = exp.e1.isDotExp())
    {
        eleft = de.e1;
        eright = de.e2;
    }
    else
    {
        eleft = null;
        eright = exp.e1;
    }

    Type t1b = exp.e1.type.toBasetype();

    if (auto ie = eright.isScopeExp()) // also used for template alias's
    {
        auto flags = SearchLocalsOnly;
        /* Disable access to another module's private imports.
         * The check for 'is sds our current module' is because
         * the current module should have access to its own imports.
         */
        if (ie.sds.isModule() && ie.sds != sc._module)
            flags |= IgnorePrivateImports;
        if (sc.flags & SCOPE.ignoresymbolvisibility)
            flags |= IgnoreSymbolVisibility;
        Dsymbol s = ie.sds.search(exp.loc, exp.ident, flags);
        /* Check for visibility before resolving aliases because public
         * aliases to private symbols are public.
         */
        if (s && !(sc.flags & SCOPE.ignoresymbolvisibility) && !symbolIsVisible(sc._module, s))
        {
            s = null;
        }
        if (s)
        {
            auto p = s.isPackage();
            if (p && checkAccess(sc, p))
            {
                s = null;
            }
        }
        if (s)
        {
            // if 's' is a tuple variable, the tuple is returned.
            s = s.toAlias();

            exp.checkDeprecated(sc, s);
            exp.checkDisabled(sc, s);

            if (auto em = s.isEnumMember())
            {
                return em.getVarExp(exp.loc, sc);
            }
            if (auto v = s.isVarDeclaration())
            {
                //printf("DotIdExp:: Identifier '%s' is a variable, type '%s'\n", toChars(), v.type.toChars());
                if (!v.type ||
                    !v.type.deco && v.inuse)
                {
                    if (v.inuse)
                        exp.error("circular reference to %s `%s`", v.kind(), v.toPrettyChars());
                    else
                        exp.error("forward reference to %s `%s`", v.kind(), v.toPrettyChars());
                    return ErrorExp.get();
                }
                if (v.type.isTypeError())
                    return ErrorExp.get();

                if ((v.storage_class & STC.manifest) && v._init && !exp.wantsym)
                {
                    /* Normally, the replacement of a symbol with its initializer is supposed to be in semantic2().
                     * Introduced by https://github.com/dlang/dmd/pull/5588 which should probably
                     * be reverted. `wantsym` is the hack to work around the problem.
                     */
                    if (v.inuse)
                    {
                        error(exp.loc, "circular initialization of %s `%s`", v.kind(), v.toPrettyChars());
                        return ErrorExp.get();
                    }
                    auto e = v.expandInitializer(exp.loc);
                    v.inuse++;
                    e = e.expressionSemantic(sc);
                    v.inuse--;
                    return e;
                }

                Expression e;
                if (v.needThis())
                {
                    if (!eleft)
                        eleft = new ThisExp(exp.loc);
                    e = new DotVarExp(exp.loc, eleft, v);
                    e = e.expressionSemantic(sc);
                }
                else
                {
                    e = new VarExp(exp.loc, v);
                    if (eleft)
                    {
                        e = new CommaExp(exp.loc, eleft, e);
                        e.type = v.type;
                    }
                }
                e = e.deref();
                return e.expressionSemantic(sc);
            }

            if (auto f = s.isFuncDeclaration())
            {
                //printf("it's a function\n");
                if (!f.functionSemantic())
                    return ErrorExp.get();
                Expression e;
                if (f.needThis())
                {
                    if (!eleft)
                        eleft = new ThisExp(exp.loc);
                    e = new DotVarExp(exp.loc, eleft, f, true);
                    e = e.expressionSemantic(sc);
                }
                else
                {
                    e = new VarExp(exp.loc, f, true);
                    if (eleft)
                    {
                        e = new CommaExp(exp.loc, eleft, e);
                        e.type = f.type;
                    }
                }
                return e;
            }
            if (auto td = s.isTemplateDeclaration())
            {
                Expression e;
                if (eleft)
                    e = new DotTemplateExp(exp.loc, eleft, td);
                else
                    e = new TemplateExp(exp.loc, td);
                e = e.expressionSemantic(sc);
                return e;
            }
            if (OverDeclaration od = s.isOverDeclaration())
            {
                Expression e = new VarExp(exp.loc, od, true);
                if (eleft)
                {
                    e = new CommaExp(exp.loc, eleft, e);
                    e.type = Type.tvoid; // ambiguous type?
                }
                return e.expressionSemantic(sc);
            }
            if (auto o = s.isOverloadSet())
            {
                //printf("'%s' is an overload set\n", o.toChars());
                return new OverExp(exp.loc, o);
            }

            if (auto t = s.getType())
            {
                return (new TypeExp(exp.loc, t)).expressionSemantic(sc);
            }

            if (auto tup = s.isTupleDeclaration())
            {
                if (eleft)
                {
                    Expression e = new DotVarExp(exp.loc, eleft, tup);
                    e = e.expressionSemantic(sc);
                    return e;
                }
                Expression e = new TupleExp(exp.loc, tup);
                e = e.expressionSemantic(sc);
                return e;
            }

            if (auto sds = s.isScopeDsymbol())
            {
                //printf("it's a ScopeDsymbol %s\n", ident.toChars());
                Expression e = new ScopeExp(exp.loc, sds);
                e = e.expressionSemantic(sc);
                if (eleft)
                    e = new DotExp(exp.loc, eleft, e);
                return e;
            }

            if (auto imp = s.isImport())
            {
                Expression se = new ScopeExp(exp.loc, imp.pkg);
                return se.expressionSemantic(sc);
            }
            // BUG: handle other cases like in IdentifierExp::semantic()
            debug
            {
                printf("s = '%s', kind = '%s'\n", s.toChars(), s.kind());
            }
            assert(0);
        }
        else if (exp.ident == Id.stringof)
        {
            Expression e = new StringExp(exp.loc, ie.toString());
            e = e.expressionSemantic(sc);
            return e;
        }
        if (ie.sds.isPackage() || ie.sds.isImport() || ie.sds.isModule())
        {
            flag = 0;
        }
        if (flag)
            return null;
        s = ie.sds.search_correct(exp.ident);
        if (s && symbolIsVisible(sc, s))
        {
            if (s.isPackage())
                exp.error("undefined identifier `%s` in %s `%s`, perhaps add `static import %s;`", exp.ident.toChars(), ie.sds.kind(), ie.sds.toPrettyChars(), s.toPrettyChars());
            else
                exp.error("undefined identifier `%s` in %s `%s`, did you mean %s `%s`?", exp.ident.toChars(), ie.sds.kind(), ie.sds.toPrettyChars(), s.kind(), s.toChars());
        }
        else
            exp.error("undefined identifier `%s` in %s `%s`", exp.ident.toChars(), ie.sds.kind(), ie.sds.toPrettyChars());
        return ErrorExp.get();
    }
    else if (t1b.ty == Tpointer && exp.e1.type.ty != Tenum &&
             !(
               exp.ident == Id.__sizeof ||
               exp.ident == Id.__xalignof ||
               !cfile &&
                (exp.ident == Id._mangleof ||
                 exp.ident == Id.offsetof ||
                 exp.ident == Id._init ||
                 exp.ident == Id.stringof)
              ))
    {
        Type t1bn = t1b.nextOf();
        if (flag)
        {
            if (AggregateDeclaration ad = isAggregate(t1bn))
            {
                if (!ad.members) // https://issues.dlang.org/show_bug.cgi?id=11312
                    return null;
            }
        }

        /* Rewrite:
         *   p.ident
         * as:
         *   (*p).ident
         */
        if (flag && t1bn.ty == Tvoid)
            return null;
        Expression e = new PtrExp(exp.loc, exp.e1);
        e = e.expressionSemantic(sc);
        return e.type.dotExp(sc, e, exp.ident, flag | (exp.noderef ? DotExpFlag.noDeref : 0));
    }
    else if (exp.ident == Id.__xalignof &&
             exp.e1.isVarExp() &&
             exp.e1.isVarExp().var.isVarDeclaration() &&
             !exp.e1.isVarExp().var.isVarDeclaration().alignment.isUnknown())
    {
        // For `x.alignof` get the alignment of the variable, not the alignment of its type
        const explicitAlignment = exp.e1.isVarExp().var.isVarDeclaration().alignment;
        const naturalAlignment = exp.e1.type.alignsize();
        const actualAlignment = explicitAlignment.isDefault() ? naturalAlignment : explicitAlignment.get();
        Expression e = new IntegerExp(exp.loc, actualAlignment, Type.tsize_t);
        return e;
    }
    else if ((exp.ident == Id.max || exp.ident == Id.min) &&
             exp.e1.isVarExp() &&
             exp.e1.isVarExp().var.isBitFieldDeclaration())
    {
        // For `x.max` and `x.min` get the max/min of the bitfield, not the max/min of its type
        auto bf = exp.e1.isVarExp().var.isBitFieldDeclaration();
        return new IntegerExp(exp.loc, bf.getMinMax(exp.ident), bf.type);
    }
    else if ((exp.ident == Id.max || exp.ident == Id.min) &&
             exp.e1.isDotVarExp() &&
             exp.e1.isDotVarExp().var.isBitFieldDeclaration())
    {
        // For `x.max` and `x.min` get the max/min of the bitfield, not the max/min of its type
        auto bf = exp.e1.isDotVarExp().var.isBitFieldDeclaration();
        return new IntegerExp(exp.loc, bf.getMinMax(exp.ident), bf.type);
    }
    else
    {
        if (exp.e1.isTypeExp() || exp.e1.isTemplateExp())
            flag = 0;
        Expression e = exp.e1.type.dotExp(sc, exp.e1, exp.ident, flag | (exp.noderef ? DotExpFlag.noDeref : 0));
        if (e)
        {
            e = e.expressionSemantic(sc);
        }
        return e;
    }
}

// Resolve e1.ident!tiargs without seeing UFCS.
// If flag == 1, stop "not a property" error and return NULL.
Expression dotTemplateSemanticProp(DotTemplateInstanceExp exp, Scope* sc, int flag)
{
    static if (LOGSEMANTIC)
    {
        printf("DotTemplateInstanceExpY::semantic('%s')\n", exp.toChars());
    }

    static Expression errorExp()
    {
        return ErrorExp.get();
    }

    Expression e1 = exp.e1;

    if (exp.ti.tempdecl && exp.ti.tempdecl.parent && exp.ti.tempdecl.parent.isTemplateMixin())
    {
        // if 'ti.tempdecl' happens to be found in a mixin template don't lose that info
        // and do the symbol search in that context (Issue: 19476)
        auto tm = cast(TemplateMixin)exp.ti.tempdecl.parent;
        e1 = new DotExp(exp.e1.loc, exp.e1, new ScopeExp(tm.loc, tm));
    }

    auto die = new DotIdExp(exp.loc, e1, exp.ti.name);

    Expression e = die.dotIdSemanticPropX(sc);
    if (e == die)
    {
        exp.e1 = die.e1; // take back
        Type t1b = exp.e1.type.toBasetype();
        if (t1b.ty == Tarray || t1b.ty == Tsarray || t1b.ty == Taarray || t1b.ty == Tnull || (t1b.isTypeBasic() && t1b.ty != Tvoid))
        {
            /* No built-in type has templatized properties, so do shortcut.
             * It is necessary in: 1024.max!"a < b"
             */
            if (flag)
                return null;
        }
        e = die.dotIdSemanticProp(sc, flag);
        if (flag)
        {
            if (!e ||
                isDotOpDispatch(e))
            {
                /* opDispatch!tiargs would be a function template that needs IFTI,
                 * so it's not a template
                 */
                return null;
            }
        }
    }
    assert(e);

    if (e.op == EXP.error)
        return e;
    if (DotVarExp dve = e.isDotVarExp())
    {
        if (FuncDeclaration fd = dve.var.isFuncDeclaration())
        {
            if (TemplateDeclaration td = fd.findTemplateDeclRoot())
            {
                e = new DotTemplateExp(dve.loc, dve.e1, td);
                e = e.expressionSemantic(sc);
            }
        }
        else if (OverDeclaration od = dve.var.isOverDeclaration())
        {
            exp.e1 = dve.e1; // pull semantic() result

            if (!exp.findTempDecl(sc))
                goto Lerr;
            if (exp.ti.needsTypeInference(sc))
                return exp;
            exp.ti.dsymbolSemantic(sc);
            if (!exp.ti.inst || exp.ti.errors) // if template failed to expand
                return errorExp();

            if (Declaration v = exp.ti.toAlias().isDeclaration())
            {
                if (v.type && !v.type.deco)
                    v.type = v.type.typeSemantic(v.loc, sc);
                return new DotVarExp(exp.loc, exp.e1, v)
                       .expressionSemantic(sc);
            }
            return new DotExp(exp.loc, exp.e1, new ScopeExp(exp.loc, exp.ti))
                   .expressionSemantic(sc);
        }
    }
    else if (e.op == EXP.variable)
    {
        VarExp ve = cast(VarExp)e;
        if (FuncDeclaration fd = ve.var.isFuncDeclaration())
        {
            if (TemplateDeclaration td = fd.findTemplateDeclRoot())
            {
                e = new TemplateExp(ve.loc, td)
                    .expressionSemantic(sc);
            }
        }
        else if (OverDeclaration od = ve.var.isOverDeclaration())
        {
            exp.ti.tempdecl = od;
            return new ScopeExp(exp.loc, exp.ti)
                   .expressionSemantic(sc);
        }
    }

    if (DotTemplateExp dte = e.isDotTemplateExp())
    {
        exp.e1 = dte.e1; // pull semantic() result

        exp.ti.tempdecl = dte.td;
        if (!exp.ti.semanticTiargs(sc))
            return errorExp();
        if (exp.ti.needsTypeInference(sc))
            return exp;
        exp.ti.dsymbolSemantic(sc);
        if (!exp.ti.inst || exp.ti.errors) // if template failed to expand
            return errorExp();

        if (Declaration v = exp.ti.toAlias().isDeclaration())
        {
            return new DotVarExp(exp.loc, exp.e1, v)
                   .expressionSemantic(sc);
        }
        return new DotExp(exp.loc, exp.e1, new ScopeExp(exp.loc, exp.ti))
               .expressionSemantic(sc);
    }
    else if (e.op == EXP.template_)
    {
        exp.ti.tempdecl = (cast(TemplateExp)e).td;
        return new ScopeExp(exp.loc, exp.ti)
               .expressionSemantic(sc);
    }
    else if (DotExp de = e.isDotExp())
    {
        if (de.e2.op == EXP.overloadSet)
        {
            if (!exp.findTempDecl(sc) || !exp.ti.semanticTiargs(sc))
            {
                return errorExp();
            }
            if (exp.ti.needsTypeInference(sc))
                return exp;
            exp.ti.dsymbolSemantic(sc);
            if (!exp.ti.inst || exp.ti.errors) // if template failed to expand
                return errorExp();

            if (Declaration v = exp.ti.toAlias().isDeclaration())
            {
                if (v.type && !v.type.deco)
                    v.type = v.type.typeSemantic(v.loc, sc);
                return new DotVarExp(exp.loc, exp.e1, v)
                       .expressionSemantic(sc);
            }
            return new DotExp(exp.loc, exp.e1, new ScopeExp(exp.loc, exp.ti))
                   .expressionSemantic(sc);
        }
    }
    else if (OverExp oe = e.isOverExp())
    {
        exp.ti.tempdecl = oe.vars;
        return new ScopeExp(exp.loc, exp.ti)
               .expressionSemantic(sc);
    }

Lerr:
    exp.error("`%s` isn't a template", e.toChars());
    return errorExp();
}

/***************************************
 * If expression is shared, check that we can access it.
 * Give error message if not.
 *
 * Params:
 *      e = expression to check
 *      sc = context
 *      returnRef = Whether this expression is for a `return` statement
 *                  off a `ref` function, in which case a single level
 *                  of dereference is allowed (e.g. `shared(int)*`).
 * Returns:
 *      true on error
 */
bool checkSharedAccess(Expression e, Scope* sc, bool returnRef = false)
{
    if (global.params.noSharedAccess != FeatureState.enabled ||
        !sc ||
        sc.intypeof ||
        sc.flags & SCOPE.ctfe)
    {
        return false;
    }

    //printf("checkSharedAccess() `%s` returnRef: %d\n", e.toChars(), returnRef);

    bool check(Expression e, bool allowRef)
    {
        bool sharedError(Expression e)
        {
            // https://dlang.org/phobos/core_atomic.html
            e.error("direct access to shared `%s` is not allowed, see `core.atomic`", e.toChars());
            return true;
        }

        // Error by default
        bool visit(Expression e)
        {
            // https://issues.dlang.org/show_bug.cgi?id=23639
            // Should be able to cast(shared)
            if (!e.isCastExp() && e.type.isShared())
                return sharedError(e);
            return false;
        }

        bool visitNew(NewExp e)
        {
            if (e.thisexp)
                check(e.thisexp, false);
            return false;
        }

        bool visitVar(VarExp e)
        {
            // https://issues.dlang.org/show_bug.cgi?id=20908
            // direct access to init symbols is ok as they
            // cannot be modified.
            if (e.var.isSymbolDeclaration())
                return false;

            // https://issues.dlang.org/show_bug.cgi?id=22626
            // Synchronized functions don't need to use core.atomic
            // when accessing `this`.
            if (sc.func && sc.func.isSynchronized())
            {
                if (e.var.isThisDeclaration())
                    return false;
                else
                    return sharedError(e);
            }
            else if (!allowRef && e.var.type.isShared())
                return sharedError(e);

            return false;
        }

        bool visitAddr(AddrExp e)
        {
            return check(e.e1, true);
        }

        bool visitPtr(PtrExp e)
        {
            if (!allowRef && e.type.isShared())
                return sharedError(e);

            if (e.e1.type.isShared())
                return sharedError(e);

            return check(e.e1, false);
        }

        bool visitDotVar(DotVarExp e)
        {
            //printf("dotvarexp = %s\n", e.toChars());
            if (e.type.isShared())
            {
                if (e.e1.isThisExp())
                {
                    // https://issues.dlang.org/show_bug.cgi?id=22626
                    if (sc.func && sc.func.isSynchronized())
                        return false;

                    // https://issues.dlang.org/show_bug.cgi?id=23790
                    if (e.e1.type.isTypeStruct())
                        return false;
                }

                auto fd = e.var.isFuncDeclaration();
                const sharedFunc = fd && fd.type.isShared;
                if (!allowRef && !sharedFunc)
                    return sharedError(e);

                // Allow using `DotVarExp` within value types
                if (e.e1.type.isTypeSArray() || e.e1.type.isTypeStruct())
                    return check(e.e1, allowRef);

                // If we end up with a single `VarExp`, it might be a `ref` param
                // `shared ref T` param == `shared(T)*`.
                if (auto ve = e.e1.isVarExp())
                {
                    return check(e.e1, allowRef && (ve.var.storage_class & STC.ref_));
                }

                return sharedError(e);
            }

            return check(e.e1, false);
        }

        bool visitIndex(IndexExp e)
        {
            if (!allowRef && e.type.isShared())
                return sharedError(e);

            if (e.e1.type.isShared())
                return sharedError(e);

            return check(e.e1, false);
        }

        bool visitComma(CommaExp e)
        {
            // Cannot be `return ref` since we can't use the return,
            // but it's better to show that error than an unrelated `shared` one
            return check(e.e2, true);
        }

        switch (e.op)
        {
            default:              return visit(e);

            // Those have no indirections / can be ignored
            case EXP.call:
            case EXP.error:
            case EXP.complex80:
            case EXP.int64:
            case EXP.null_:       return false;

            case EXP.variable:    return visitVar(e.isVarExp());
            case EXP.new_:        return visitNew(e.isNewExp());
            case EXP.address:     return visitAddr(e.isAddrExp());
            case EXP.star:        return visitPtr(e.isPtrExp());
            case EXP.dotVariable: return visitDotVar(e.isDotVarExp());
            case EXP.index:       return visitIndex(e.isIndexExp());
        }
    }

    return check(e, returnRef);
}



/****************************************************
 * Determine if `exp`, which gets its address taken, can do so safely.
 * Params:
 *      sc = context
 *      exp = expression having its address taken
 *      v = the variable getting its address taken
 * Returns:
 *      `true` if ok, `false` for error
 */
bool checkAddressVar(Scope* sc, Expression exp, VarDeclaration v)
{
    //printf("checkAddressVar(exp: %s, v: %s)\n", exp.toChars(), v.toChars());
    if (v is null)
        return true;

    if (!v.canTakeAddressOf())
    {
        exp.error("cannot take address of `%s`", exp.toChars());
        return false;
    }
    if (sc.func && !sc.intypeof && !v.isDataseg())
    {
        if (global.params.useDIP1000 != FeatureState.enabled &&
            !(v.storage_class & STC.temp) &&
            sc.setUnsafe(false, exp.loc, "cannot take address of local `%s` in `@safe` function `%s`", v, sc.func))
        {
            return false;
        }
    }
    return true;
}

/**************************************
 * This check ensures that the object in `exp` can have its address taken, or
 * issue a diagnostic error.
 * Params:
 *      e = expression to check
 *      sc = context
 * Returns:
 *      true if the expression is addressable
 */
bool checkAddressable(Expression e, Scope* sc)
{
    Expression ex = e;
    while (true)
    {
        switch (ex.op)
        {
            case EXP.dotVariable:
                // https://issues.dlang.org/show_bug.cgi?id=22749
                // Error about taking address of any bit-field, regardless of
                // whether SCOPE.Cfile is set.
                if (auto bf = ex.isDotVarExp().var.isBitFieldDeclaration())
                {
                    e.error("cannot take address of bit-field `%s`", bf.toChars());
                    return false;
                }
                goto case EXP.cast_;

            case EXP.index:
                ex = ex.isBinExp().e1;
                continue;

            case EXP.address:
            case EXP.array:
            case EXP.cast_:
                ex = ex.isUnaExp().e1;
                continue;

            case EXP.variable:
                if (sc.flags & SCOPE.Cfile)
                {
                    // C11 6.5.3.2: A variable that has its address taken cannot be
                    // stored in a register.
                    // C11 6.3.2.1: An array that has its address computed with `[]`
                    // or cast to an lvalue pointer cannot be stored in a register.
                    if (ex.isVarExp().var.storage_class & STC.register)
                    {
                        if (e.isIndexExp())
                            e.error("cannot index through register variable `%s`", ex.toChars());
                        else
                            e.error("cannot take address of register variable `%s`", ex.toChars());
                        return false;
                    }
                }
                break;

            default:
                break;
        }
        break;
    }
    return true;
}


/*******************************
 * Checks the attributes of a function.
 * Purity (`pure`), safety (`@safe`), no GC allocations(`@nogc`)
 * and usage of `deprecated` and `@disabled`-ed symbols are checked.
 *
 * Params:
 *  exp = expression to check attributes for
 *  sc  = scope of the function
 *  f   = function to be checked
 * Returns: `true` if error occur.
 */
private bool checkFunctionAttributes(Expression exp, Scope* sc, FuncDeclaration f)
{
    with(exp)
    {
        bool error = checkDisabled(sc, f);
        error |= checkDeprecated(sc, f);
        error |= checkPurity(sc, f);
        error |= checkSafety(sc, f);
        error |= checkNogc(sc, f);
        return error;
    }
}

/*******************************
 * Helper function for `getRightThis()`.
 * Gets `this` of the next outer aggregate.
 * Params:
 *      loc = location to use for error messages
 *      sc = context
 *      s = the parent symbol of the existing `this`
 *      ad = struct or class we need the correct `this` for
 *      e1 = existing `this`
 *      t = type of the existing `this`
 *      var = the specific member of ad we're accessing
 *      flag = if true, return `null` instead of throwing an error
 * Returns:
 *      Expression representing the `this` for the var
 */
Expression getThisSkipNestedFuncs(const ref Loc loc, Scope* sc, Dsymbol s, AggregateDeclaration ad, Expression e1, Type t, Dsymbol var, bool flag = false)
{
    int n = 0;
    while (s && s.isFuncDeclaration())
    {
        FuncDeclaration f = s.isFuncDeclaration();
        if (f.vthis)
        {
            n++;
            e1 = new VarExp(loc, f.vthis);
            if (f.hasDualContext())
            {
                // (*__this)[i]
                if (n > 1)
                    e1 = e1.expressionSemantic(sc);
                e1 = new PtrExp(loc, e1);
                uint i = f.followInstantiationContext(ad);
                e1 = new IndexExp(loc, e1, new IntegerExp(i));
                s = f.toParentP(ad);
                continue;
            }
        }
        else
        {
            if (flag)
                return null;
            e1.error("need `this` of type `%s` to access member `%s` from static function `%s`", ad.toChars(), var.toChars(), f.toChars());
            e1 = ErrorExp.get();
            return e1;
        }
        s = s.toParent2();
    }
    if (n > 1 || e1.op == EXP.index)
        e1 = e1.expressionSemantic(sc);
    if (s && e1.type.equivalent(Type.tvoidptr))
    {
        if (auto sad = s.isAggregateDeclaration())
        {
            Type ta = sad.handleType();
            if (ta.ty == Tstruct)
                ta = ta.pointerTo();
            e1.type = ta;
        }
    }
    e1.type = e1.type.addMod(t.mod);
    return e1;
}

/*******************************
 * Make a dual-context container for use as a `this` argument.
 * Params:
 *      loc = location to use for error messages
 *      sc = current scope
 *      fd = target function that will take the `this` argument
 * Returns:
 *      Temporary closure variable.
 * Note:
 *      The function `fd` is added to the nested references of the
 *      newly created variable such that a closure is made for the variable when
 *      the address of `fd` is taken.
 */
VarDeclaration makeThis2Argument(const ref Loc loc, Scope* sc, FuncDeclaration fd)
{
    Type tthis2 = Type.tvoidptr.sarrayOf(2);
    VarDeclaration vthis2 = new VarDeclaration(loc, tthis2, Identifier.generateId("__this"), null);
    vthis2.storage_class |= STC.temp;
    vthis2.dsymbolSemantic(sc);
    vthis2.parent = sc.parent;
    // make it a closure var
    assert(sc.func);
    sc.func.closureVars.push(vthis2);
    // add `fd` to the nested refs
    vthis2.nestedrefs.push(fd);
    return vthis2;
}

/*******************************
 * Make sure that the runtime hook `id` exists.
 * Params:
 *      loc = location to use for error messages
 *      sc = current scope
 *      id = the hook identifier
 *      description = what the hook does
 *      module_ = what module the hook is located in
 * Returns:
 *      a `bool` indicating if the hook is present.
 */
bool verifyHookExist(const ref Loc loc, ref Scope sc, Identifier id, string description, Identifier module_ = Id.object)
{
    auto rootSymbol = sc.search(loc, Id.empty, null);
    if (auto moduleSymbol = rootSymbol.search(loc, module_))
        if (moduleSymbol.search(loc, id))
          return true;
    error(loc, "`%s.%s` not found. The current runtime does not support %.*s, or the runtime is corrupt.", module_.toChars(), id.toChars(), cast(int)description.length, description.ptr);
    return false;
}

/***************************************
 * Fit elements[] to the corresponding types of the `sd`'s fields.
 *
 * Params:
 *      sd = the struct declaration
 *      loc = location to use for error messages
 *      sc = context
 *      elements = explicit arguments used to construct object
 *      stype = the constructed object type.
 * Returns:
 *      false if any errors occur,
 *      otherwise true and elements[] are rewritten for the output.
 */
private bool fit(StructDeclaration sd, const ref Loc loc, Scope* sc, Expressions* elements, Type stype)
{
    if (!elements)
        return true;

    const nfields = sd.nonHiddenFields();
    size_t offset = 0;
    for (size_t i = 0; i < elements.length; i++)
    {
        Expression e = (*elements)[i];
        if (!e)
            continue;

        e = resolveProperties(sc, e);
        if (i >= nfields)
        {
            if (i < sd.fields.length && e.op == EXP.null_)
            {
                // CTFE sometimes creates null as hidden pointer; we'll allow this.
                continue;
            }
                .error(loc, "more initializers than fields (%llu) of `%s`", cast(ulong)nfields, sd.toChars());
            return false;
        }
        VarDeclaration v = sd.fields[i];
        if (v.offset < offset)
        {
            .error(loc, "overlapping initialization for `%s`", v.toChars());
            if (!sd.isUnionDeclaration())
            {
                enum errorMsg = "`struct` initializers that contain anonymous unions" ~
                    " must initialize only the first member of a `union`. All subsequent" ~
                    " non-overlapping fields are default initialized";
                .errorSupplemental(loc, errorMsg);
            }
            return false;
        }
        const vsize = v.type.size();
        if (vsize == SIZE_INVALID)
            return false;
        offset = cast(uint)(v.offset + vsize);

        Type t = v.type;
        if (stype)
            t = t.addMod(stype.mod);
        Type origType = t;
        Type tb = t.toBasetype();

        const hasPointers = tb.hasPointers();
        if (hasPointers)
        {
            if ((!stype.alignment.isDefault() && stype.alignment.get() < target.ptrsize ||
                 (v.offset & (target.ptrsize - 1))) &&
                (sc.setUnsafe(false, loc,
                    "field `%s.%s` cannot assign to misaligned pointers in `@safe` code", sd, v)))
            {
                return false;
            }
        }

        /* Look for case of initializing a static array with a too-short
         * string literal, such as:
         *  char[5] foo = "abc";
         * Allow this by doing an explicit cast, which will lengthen the string
         * literal.
         */
        if (e.op == EXP.string_ && tb.ty == Tsarray)
        {
            StringExp se = cast(StringExp)e;
            Type typeb = se.type.toBasetype();
            TY tynto = tb.nextOf().ty;
            if (!se.committed &&
                (typeb.ty == Tarray || typeb.ty == Tsarray) && tynto.isSomeChar &&
                se.numberOfCodeUnits(tynto) < (cast(TypeSArray)tb).dim.toInteger())
            {
                e = se.castTo(sc, t);
                goto L1;
            }
        }

        while (!e.implicitConvTo(t) && tb.ty == Tsarray)
        {
            /* Static array initialization, as in:
             *  T[3][5] = e;
             */
            t = tb.nextOf();
            tb = t.toBasetype();
        }
        if (!e.implicitConvTo(t))
            t = origType; // restore type for better diagnostic

        e = e.implicitCastTo(sc, t);
    L1:
        if (e.op == EXP.error)
            return false;

        (*elements)[i] = doCopyOrMove(sc, e);
    }
    return true;
}


/**
 * Returns `em` as a VariableExp
 * Params:
 *     em = the EnumMember to wrap
 *     loc = location of use of em
 *     sc = scope of use of em
 * Returns:
 *     VarExp referenceing `em` or ErrorExp if `em` if disabled/deprecated
 */
Expression getVarExp(EnumMember em, const ref Loc loc, Scope* sc)
{
    dsymbolSemantic(em, sc);
    if (em.errors)
        return ErrorExp.get();
    em.checkDisabled(loc, sc);

    if (em.depdecl && !em.depdecl._scope)
        em.depdecl._scope = sc;
    em.checkDeprecated(loc, sc);

    if (em.errors)
        return ErrorExp.get();
    Expression e = new VarExp(loc, em);
    e = e.expressionSemantic(sc);
    if (!(sc.flags & SCOPE.Cfile) && em.isCsymbol())
    {
        /* C11 types them as int. But if in D file,
         * type qualified names as the enum
         */
        e.type = em.parent.isEnumDeclaration().type;
        assert(e.type);
    }
    return e;
}


/*****************************
 * Try to treat `exp` as a boolean,
 * Params:
 *     exp = the expression
 *     sc = scope to evalute `exp` in
 * Returns:
 *     Modified expression on success, ErrorExp on error
 */
Expression toBoolean(Expression exp, Scope* sc)
{
    switch(exp.op)
    {
        case EXP.delete_:
            exp.error("`delete` does not give a boolean result");
            return ErrorExp.get();

        case EXP.comma:
            auto ce = exp.isCommaExp();
            auto ex2 = ce.e2.toBoolean(sc);
            if (ex2.op == EXP.error)
                return ex2;
            ce.e2 = ex2;
            ce.type = ce.e2.type;
            return ce;

        case EXP.assign:
        case EXP.construct:
        case EXP.blit:
            if (sc.flags & SCOPE.Cfile)
                return exp;
            // Things like:
            //  if (a = b) ...
            // are usually mistakes.
            exp.error("assignment cannot be used as a condition, perhaps `==` was meant?");
            return ErrorExp.get();

        //LogicalExp
        case EXP.andAnd:
        case EXP.orOr:
            auto le = exp.isLogicalExp();
            auto ex2 = le.e2.toBoolean(sc);
            if (ex2.op == EXP.error)
                return ex2;
            le.e2 = ex2;
            return le;

        case EXP.question:
            auto ce = exp.isCondExp();
            auto ex1 = ce.e1.toBoolean(sc);
            auto ex2 = ce.e2.toBoolean(sc);
            if (ex1.op == EXP.error)
                return ex1;
            if (ex2.op == EXP.error)
                return ex2;
            ce.e1 = ex1;
            ce.e2 = ex2;
            return ce;


        default:
            // Default is 'yes' - do nothing
            Expression e = arrayFuncConv(exp, sc);
            Type t = e.type;
            Type tb = t.toBasetype();
            Type att = null;

            while (1)
            {
                // Structs can be converted to bool using opCast(bool)()
                if (auto ts = tb.isTypeStruct())
                {
                    AggregateDeclaration ad = ts.sym;
                    /* Don't really need to check for opCast first, but by doing so we
                     * get better error messages if it isn't there.
                     */
                    if (Dsymbol fd = search_function(ad, Id._cast))
                    {
                        e = new CastExp(exp.loc, e, Type.tbool);
                        e = e.expressionSemantic(sc);
                        return e;
                    }

                    // Forward to aliasthis.
                    if (ad.aliasthis && !isRecursiveAliasThis(att, tb))
                    {
                        e = resolveAliasThis(sc, e);
                        t = e.type;
                        tb = e.type.toBasetype();
                        continue;
                    }
                }
                break;
            }

            if (!t.isBoolean())
            {
                if (tb != Type.terror)
                    exp.error("expression `%s` of type `%s` does not have a boolean value",
                              exp.toChars(), t.toChars());
                return ErrorExp.get();
            }
            return e;
    }
}
