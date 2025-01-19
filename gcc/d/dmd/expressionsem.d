/**
 * Semantic analysis of expressions.
 *
 * Specification: ($LINK2 https://dlang.org/spec/expression.html, Expressions)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
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
import dmd.deps;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dstruct;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.enumsem;
import dmd.escape;
import dmd.expression;
import dmd.file_manager;
import dmd.func;
import dmd.funcsem;
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
import dmd.mangle;
import dmd.mtype;
import dmd.mustuse;
import dmd.nspace;
import dmd.nogc;
import dmd.objc;
import dmd.opover;
import dmd.optimize;
import dmd.parse;
import dmd.printast;
import dmd.root.array;
import dmd.root.ctfloat;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.root.utf;
import dmd.semantic2;
import dmd.semantic3;
import dmd.sideeffect;
import dmd.safe;
import dmd.target;
import dmd.templatesem : matchWithInstance;
import dmd.tokens;
import dmd.traits;
import dmd.typesem;
import dmd.typinf;
import dmd.utils;
import dmd.utils : arrayCastBigEndian;
import dmd.visitor;
import dmd.visitor.postorder;

enum LOGSEMANTIC = false;

/***********************************
 * Determine if a `this` is needed to access `d`.
 * Params:
 *      sc = context
 *      d = declaration to check
 * Returns:
 *      true means a `this` is needed
 */
private bool isNeedThisScope(Scope* sc, Declaration d)
{
    if (sc.intypeof == 1)
        return false;

    AggregateDeclaration ad = d.isThis();
    if (!ad)
        return false;
    //printf("d = %s, ad = %s\n", d.toChars(), ad.toChars());

    for (Dsymbol s = sc.parent; s; s = s.toParentLocal())
    {
        //printf("\ts = %s %s, toParent2() = %p\n", s.kind(), s.toChars(), s.toParent2());
        if (AggregateDeclaration ad2 = s.isAggregateDeclaration())
        {
            if (ad2 == ad)
                return false;
            if (ad2.isNested())
                continue;
            return true;
        }
        if (FuncDeclaration f = s.isFuncDeclaration())
        {
            if (f.isMemberLocal())
                break;
        }
    }
    return true;
}


/********************************************************
 * Perform semantic analysis and CTFE on expressions to produce
 * a string.
 * Params:
 *      buf = append generated string to buffer
 *      sc = context
 *      exps = array of Expressions
 *      loc = location of the pragma / mixin where this conversion was requested, for supplemental error
 *      fmt = format string for supplemental error. May contain 1 `%s` which prints the faulty expression
 *      expandTuples = whether tuples should be expanded rather than printed as tuple syntax
 * Returns:
 *      true on error
 */
bool expressionsToString(ref OutBuffer buf, Scope* sc, Expressions* exps,
    Loc loc, const(char)* fmt, bool expandTuples)
{
    if (!exps)
        return false;

    foreach (ex; *exps)
    {
        bool error()
        {
            if (loc != Loc.initial && fmt)
                errorSupplemental(loc, fmt, ex.toChars());
            return true;
        }
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
            return error();

        // expand tuple
        if (expandTuples)
            if (auto te = e4.isTupleExp())
            {
                if (expressionsToString(buf, sc, te.exps, loc, fmt, true))
                    return error();
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

        StringExp se = e4.toStringExp();

        if (se && se.type.nextOf().ty.isSomeChar)
            buf.writestring(se.toUTF8(sc).peekString());
        else if (!(se && se.len == 0)) // don't print empty array literal `[]`
            buf.writestring(e4.toString());
    }
    return false;
}

/*****************************************
 * Determine if `this` is available by walking up the enclosing
 * scopes until a function is found.
 *
 * Params:
 *      sc = where to start looking for the enclosing function
 * Returns:
 *      Found function if it satisfies `isThis()`, otherwise `null`
 */
FuncDeclaration hasThis(Scope* sc)
{
    //printf("hasThis()\n");
    Dsymbol p = sc.parent;
    while (p && p.isTemplateMixin())
        p = p.parent;
    FuncDeclaration fdthis = p ? p.isFuncDeclaration() : null;
    //printf("fdthis = %p, '%s'\n", fdthis, fdthis ? fdthis.toChars() : "");

    // Go upwards until we find the enclosing member function
    FuncDeclaration fd = fdthis;
    while (1)
    {
        if (!fd)
        {
            return null;
        }
        if (!fd.isNested() || fd.isThis() || (fd.hasDualContext() && fd.isMember2()))
            break;

        Dsymbol parent = fd.parent;
        while (1)
        {
            if (!parent)
                return null;
            TemplateInstance ti = parent.isTemplateInstance();
            if (ti)
                parent = ti.parent;
            else
                break;
        }
        fd = parent.isFuncDeclaration();
    }

    if (!fd.isThis() && !(fd.hasDualContext() && fd.isMember2()))
    {
        return null;
    }

    assert(fd.vthis);
    return fd;

}

extern (D) bool findTempDecl(DotTemplateInstanceExp exp, Scope* sc)
{
    auto ti = exp.ti;
    auto e1 = exp.e1;
    static if (LOGSEMANTIC)
    {
        printf("DotTemplateInstanceExp::findTempDecl('%s')\n", exp.toChars());
    }
    if (ti.tempdecl)
        return true;

    Expression e = new DotIdExp(exp.loc, e1, ti.name);
    e = e.expressionSemantic(sc);
    if (e.op == EXP.dot)
        e = (cast(DotExp)e).e2;

    Dsymbol s = null;
    switch (e.op)
    {
    case EXP.overloadSet:
        s = (cast(OverExp)e).vars;
        break;

    case EXP.dotTemplateDeclaration:
        s = (cast(DotTemplateExp)e).td;
        break;

    case EXP.scope_:
        s = (cast(ScopeExp)e).sds;
        break;

    case EXP.dotVariable:
        s = (cast(DotVarExp)e).var;
        break;

    case EXP.variable:
        s = (cast(VarExp)e).var;
        break;

    default:
        return false;
    }
    return ti.updateTempDecl(sc, s);
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

    if (auto se = e.toStringExp())
        return se;
    error(exp.loc, "`string` expected for %s, not `(%s)` of type `%s`",
          s, exp.toChars(), exp.type.toChars());
    return null;
}

/****************************************
 * Convert string to char[].
 */
StringExp toUTF8(StringExp se, Scope* sc)
{
    if (se.sz == 1)
        return se;
    // Convert to UTF-8 string
    se.committed = false;
    Expression e = castTo(se, sc, Type.tchar.arrayOf());
    e = e.optimize(WANTvalue);
    auto result = e.isStringExp();
    assert(result);
    assert(result.sz == 1);
    return result;
}
/********************************
 * The type for a unary expression is incompatible.
 * Print error message.
 * Returns:
 *  ErrorExp
 */
private Expression incompatibleTypes(UnaExp e)
{
    if (e.e1.type.toBasetype() == Type.terror)
        return e.e1;

    if (e.e1.op == EXP.type)
    {
        error(e.loc, "incompatible type for `%s(%s)`: cannot use `%s` with types", EXPtoString(e.op).ptr, e.e1.toChars(), EXPtoString(e.op).ptr);
    }
    else
    {
        error(e.loc, "incompatible type for `%s(%s)`: `%s`", EXPtoString(e.op).ptr, e.e1.toChars(), e.e1.type.toChars());
    }
    return ErrorExp.get();
}

/********************************
 * The types for a binary expression are incompatible.
 * Print error message.
 * Returns:
 *  ErrorExp
 */
extern (D) Expression incompatibleTypes(BinExp e)
{
    if (e.e1.type.toBasetype() == Type.terror)
        return e.e1;
    if (e.e2.type.toBasetype() == Type.terror)
        return e.e2;

    // CondExp uses 'a ? b : c' but we're comparing 'b : c'
    const(char)* thisOp = (e.op == EXP.question) ? ":" : EXPtoString(e.op).ptr;
    if (e.e1.op == EXP.type || e.e2.op == EXP.type)
    {
        error(e.loc, "incompatible types for `(%s) %s (%s)`: cannot use `%s` with types",
            e.e1.toChars(), thisOp, e.e2.toChars(), EXPtoString(e.op).ptr);
    }
    else if (e.e1.type.equals(e.e2.type))
    {
        error(e.loc, "incompatible types for `(%s) %s (%s)`: both operands are of type `%s`",
            e.e1.toChars(), thisOp, e.e2.toChars(), e.e1.type.toChars());
    }
    else
    {
        auto ts = toAutoQualChars(e.e1.type, e.e2.type);
        error(e.loc, "incompatible types for `(%s) %s (%s)`: `%s` and `%s`",
            e.e1.toChars(), thisOp, e.e2.toChars(), ts[0], ts[1]);
    }
    return ErrorExp.get();
}

private Expression reorderSettingAAElem(BinExp exp, Scope* sc)
{
    BinExp be = exp;

    auto ie = be.e1.isIndexExp();
    if (!ie)
        return be;
    if (ie.e1.type.toBasetype().ty != Taarray)
        return be;

    /* Fix evaluation order of setting AA element
     * https://issues.dlang.org/show_bug.cgi?id=3825
     * Rewrite:
     *     aa[k1][k2][k3] op= val;
     * as:
     *     auto ref __aatmp = aa;
     *     auto ref __aakey3 = k1, __aakey2 = k2, __aakey1 = k3;
     *     auto ref __aaval = val;
     *     __aatmp[__aakey3][__aakey2][__aakey1] op= __aaval;  // assignment
     */

    Expression e0;
    while (1)
    {
        Expression de;
        ie.e2 = extractSideEffect(sc, "__aakey", de, ie.e2);
        e0 = Expression.combine(de, e0);

        auto ie1 = ie.e1.isIndexExp();
        if (!ie1 ||
            ie1.e1.type.toBasetype().ty != Taarray)
        {
            break;
        }
        ie = ie1;
    }
    assert(ie.e1.type.toBasetype().ty == Taarray);

    Expression de;
    ie.e1 = extractSideEffect(sc, "__aatmp", de, ie.e1);
    e0 = Expression.combine(de, e0);

    be.e2 = extractSideEffect(sc, "__aaval", e0, be.e2, true);

    //printf("-e0 = %s, be = %s\n", e0.toChars(), be.toChars());
    return Expression.combine(e0, be);
}

private Expression checkOpAssignTypes(BinExp binExp, Scope* sc)
{
    auto e1 = binExp.e1;
    auto e2 = binExp.e2;
    auto op = binExp.op;
    auto type = binExp.type;
    auto loc = binExp.loc;

    // At that point t1 and t2 are the merged types. type is the original type of the lhs.
    Type t1 = e1.type;
    Type t2 = e2.type;

    // T opAssign floating yields a floating. Prevent truncating conversions (float to int).
    // See https://issues.dlang.org/show_bug.cgi?id=3841.
    // Should we also prevent double to float (type.isFloating() && type.size() < t2.size()) ?
    if (op == EXP.addAssign || op == EXP.minAssign ||
        op == EXP.mulAssign || op == EXP.divAssign || op == EXP.modAssign ||
        op == EXP.powAssign)
    {
        if ((type.isIntegral() && t2.isFloating()))
        {
            warning(loc, "`%s %s %s` is performing truncating conversion", type.toChars(), EXPtoString(op).ptr, t2.toChars());
        }
    }

    // generate an error if this is a nonsensical *=,/=, or %=, eg real *= imaginary
    if (op == EXP.mulAssign || op == EXP.divAssign || op == EXP.modAssign)
    {
        // Any multiplication by an imaginary or complex number yields a complex result.
        // r *= c, i*=c, r*=i, i*=i are all forbidden operations.
        const(char)* opstr = EXPtoString(op).ptr;
        if (t1.isReal() && t2.isComplex())
        {
            error(loc, "`%s %s %s` is undefined. Did you mean `%s %s %s.re`?", t1.toChars(), opstr, t2.toChars(), t1.toChars(), opstr, t2.toChars());
            return ErrorExp.get();
        }
        else if (t1.isImaginary() && t2.isComplex())
        {
            error(loc, "`%s %s %s` is undefined. Did you mean `%s %s %s.im`?", t1.toChars(), opstr, t2.toChars(), t1.toChars(), opstr, t2.toChars());
            return ErrorExp.get();
        }
        else if ((t1.isReal() || t1.isImaginary()) && t2.isImaginary())
        {
            error(loc, "`%s %s %s` is an undefined operation", t1.toChars(), opstr, t2.toChars());
            return ErrorExp.get();
        }
    }

    // generate an error if this is a nonsensical += or -=, eg real += imaginary
    if (op == EXP.addAssign || op == EXP.minAssign)
    {
        // Addition or subtraction of a real and an imaginary is a complex result.
        // Thus, r+=i, r+=c, i+=r, i+=c are all forbidden operations.
        if ((t1.isReal() && (t2.isImaginary() || t2.isComplex())) || (t1.isImaginary() && (t2.isReal() || t2.isComplex())))
        {
            error(loc, "`%s %s %s` is undefined (result is complex)", t1.toChars(), EXPtoString(op).ptr, t2.toChars());
            return ErrorExp.get();
        }
        if (type.isReal() || type.isImaginary())
        {
            assert(global.errors || t2.isFloating());
            e2 = e2.castTo(sc, t1);
        }
    }
    if (op == EXP.mulAssign && t2.isFloating())
    {
        if (t1.isReal())
        {
            if (t2.isImaginary() || t2.isComplex())
            {
                e2 = e2.castTo(sc, t1);
            }
        }
        else if (t1.isImaginary())
        {
            if (t2.isImaginary() || t2.isComplex())
            {
                switch (t1.ty)
                {
                case Timaginary32:
                    t2 = Type.tfloat32;
                    break;

                case Timaginary64:
                    t2 = Type.tfloat64;
                    break;

                case Timaginary80:
                    t2 = Type.tfloat80;
                    break;

                default:
                    assert(0);
                }
                e2 = e2.castTo(sc, t2);
            }
        }
    }
    else if (op == EXP.divAssign && t2.isImaginary())
    {
        if (t1.isReal())
        {
            // x/iv = i(-x/v)
            // Therefore, the result is 0
            e2 = new CommaExp(loc, e2, new RealExp(loc, CTFloat.zero, t1));
            e2.type = t1;
            Expression e = new AssignExp(loc, e1, e2);
            e.type = t1;
            return e;
        }
        else if (t1.isImaginary())
        {
            Type t3;
            switch (t1.ty)
            {
            case Timaginary32:
                t3 = Type.tfloat32;
                break;

            case Timaginary64:
                t3 = Type.tfloat64;
                break;

            case Timaginary80:
                t3 = Type.tfloat80;
                break;

            default:
                assert(0);
            }
            e2 = e2.castTo(sc, t3);
            Expression e = new AssignExp(loc, e1, e2);
            e.type = t1;
            return e;
        }
    }
    else if (op == EXP.modAssign)
    {
        if (t2.isComplex())
        {
            error(loc, "cannot perform modulo complex arithmetic");
            return ErrorExp.get();
        }
    }
    return binExp;
}

private Expression extractOpDollarSideEffect(Scope* sc, UnaExp ue)
{
    Expression e0;
    Expression e1 = Expression.extractLast(ue.e1, e0);
    // https://issues.dlang.org/show_bug.cgi?id=12585
    // Extract the side effect part if ue.e1 is comma.

    if (sc.ctfe ? hasSideEffect(e1) : !isTrivialExp(e1)) // match logic in extractSideEffect()
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

/****************************************
 * Expand alias this tuples.
 */
TupleDeclaration isAliasThisTuple(Expression e)
{
    if (!e.type)
        return null;

    Type t = e.type.toBasetype();
    while (true)
    {
        Dsymbol s = t.toDsymbol(null);
        if (!s)
            return null;
        auto ad = s.isAggregateDeclaration();
        if (!ad)
            return null;
        s = ad.aliasthis ? ad.aliasthis.sym : null;
        if (s && s.isVarDeclaration())
        {
            TupleDeclaration td = s.isVarDeclaration().toAlias().isTupleDeclaration();
            if (td && td.isexp)
                return td;
        }
        if (Type att = t.aliasthisOf())
            t = att;
    }
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
    Expression fallback()
    {
        if (ae.arguments.length == 1)
            return null;
        error(ae.loc, "multi-dimensional slicing requires template `opSlice`");
        return ErrorExp.get();
    }
    foreach (i, e; *ae.arguments)
    {
        if (i == 0)
            *pe0 = extractOpDollarSideEffect(sc, ae);

        if (e.op == EXP.interval && !(slice && slice.isTemplateDeclaration()))
        {
            return fallback();
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

            const xerrors = global.startGagging();
            sc = sc.push();
            FuncDeclaration fslice = resolveFuncCall(ae.loc, sc, slice, tiargs, ae.e1.type, ArgumentList(fargs), FuncResolveFlag.quiet);
            sc = sc.pop();
            global.endGagging(xerrors);
            if (!fslice)
                return fallback();

            e = new DotTemplateInstanceExp(ae.loc, ae.e1, slice.ident, tiargs);
            e = new CallExp(ae.loc, e, fargs);
            e = e.expressionSemantic(sc);
        }

        if (!e.type)
        {
            error(ae.loc, "`%s` has no value", e.toChars());
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
            error(ae.loc, "`%s` has no value", e.toChars());
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

/************************************************
 * Handle the postblit call on lvalue, or the move of rvalue.
 *
 * Params:
 *   sc = the scope where the expression is encountered
 *   e = the expression the needs to be moved or copied (source)
 *   t = if the struct defines a copy constructor, the type of the destination (can be NULL)
 *   nrvo = true if the generated copy can be treated as NRVO
 *   move = true to allow a move constructor to be used, false to prevent infinite recursion
 * Returns:
 *  The expression that copy constructs or moves the value.
 */
extern (D) Expression doCopyOrMove(Scope *sc, Expression e, Type t, bool nrvo, bool move = false)
{
    //printf("doCopyOrMove() %s\n", toChars(e));
    StructDeclaration sd;
    if (t)
    {
        if (auto ts = t.isTypeStruct())
            sd = ts.sym;
    }

    if (auto ce = e.isCondExp())
    {
        ce.e1 = doCopyOrMove(sc, ce.e1, null, nrvo);
        ce.e2 = doCopyOrMove(sc, ce.e2, null, nrvo);
    }
    else if (e.isLvalue())
    {
        e = callCpCtor(sc, e, t, nrvo);
    }
    else if (move && sd && sd.hasMoveCtor && !e.isCallExp() && !e.isStructLiteralExp())
    {
        // #move
        /* Rewrite as:
         *    S __copyrvalue;
         *    __copyrvalue.moveCtor(e);
         *    __copyrvalue;
         */
        VarDeclaration vd = new VarDeclaration(e.loc, e.type, Identifier.generateId("__copyrvalue"), null);
        if (nrvo)
            vd.adFlags |= Declaration.nrvo;
        vd.storage_class |= STC.nodtor;
        vd.dsymbolSemantic(sc);
        Expression de = new DeclarationExp(e.loc, vd);
        Expression ve = new VarExp(e.loc, vd);

        Expression er;
        er = new DotIdExp(e.loc, ve, Id.ctor);  // ve.ctor
        er = new CallExp(e.loc, er, e);         // ve.ctor(e)
        er = new CommaExp(e.loc, er, new VarExp(e.loc, vd)); // ve.ctor(e),vd
        er = Expression.combine(de, er);        // de,ve.ctor(e),vd

        e = er.expressionSemantic(sc);
    }
    else
    {
        e = valueNoDtor(e);
    }
    return e;
}

/*********************************************
 * If e is an instance of a struct, and that struct has a copy constructor,
 * rewrite e as:
 *    (tmp = e),tmp
 * Params:
 *      sc = just used to specify the scope of created temporary variable
 *      destinationType = the type of the object on which the copy constructor is called;
 *                        may be null if the struct defines a postblit
 *      nrvo = true if the generated copy can be treated as NRVO
 */
private Expression callCpCtor(Scope* sc, Expression e, Type destinationType, bool nrvo)
{
    //printf("callCpCtor(e: %s et: %s destinationType: %s\n", toChars(e), toChars(e.type), toChars(destinationType));
    auto ts = e.type.baseElemOf().isTypeStruct();

    if (!ts)
        return e;
    StructDeclaration sd = ts.sym;
    if (!sd.postblit && !sd.hasCopyCtor)
        return e;

    /* Create a variable tmp, and replace the argument e with:
     *      (tmp = e),tmp
     * and let AssignExp() handle the construction.
     * This is not the most efficient, ideally tmp would be constructed
     * directly onto the stack.
     */
    VarDeclaration tmp = copyToTemp(STC.rvalue, "__copytmp", e);
    if (nrvo)
        tmp.adFlags |= Declaration.nrvo;
    if (sd.hasCopyCtor && destinationType)
    {
        // https://issues.dlang.org/show_bug.cgi?id=22619
        // If the destination type is inout we can preserve it
        // only if inside an inout function; if we are not inside
        // an inout function, then we will preserve the type of
        // the source
        if (destinationType.hasWild && !(sc.func.storage_class & STC.wild))
            tmp.type = e.type;
        else
            tmp.type = destinationType;
    }
    tmp.storage_class |= STC.nodtor;
    tmp.dsymbolSemantic(sc);
    Expression de = new DeclarationExp(e.loc, tmp);
    Expression ve = new VarExp(e.loc, tmp);
    de.type = Type.tvoid;
    ve.type = e.type;
    return Expression.combine(de, ve);
}

/************************************************
 * If we want the value of this expression, but do not want to call
 * the destructor on it.
 */
Expression valueNoDtor(Expression e)
{
    //printf("valueNoDtor() %s\n", toChars(e));
    auto ex = lastComma(e);

    if (auto ce = ex.isCallExp())
    {
        /* The struct value returned from the function is transferred
         * so do not call the destructor on it.
         * Recognize:
         *       ((S _ctmp = S.init), _ctmp).this(...)
         * and make sure the destructor is not called on _ctmp
         * BUG: if ex is a CommaExp, we should go down the right side.
         */
        if (auto dve = ce.e1.isDotVarExp())
        {
            if (dve.var.isCtorDeclaration())
            {
                // It's a constructor call
                if (auto comma = dve.e1.isCommaExp())
                {
                    if (auto ve = comma.e2.isVarExp())
                    {
                        VarDeclaration ctmp = ve.var.isVarDeclaration();
                        if (ctmp)
                        {
                            ctmp.storage_class |= STC.nodtor;
                            assert(!ce.isLvalue());
                        }
                    }
                }
            }
        }
    }
    else if (auto ve = ex.isVarExp())
    {
        auto vtmp = ve.var.isVarDeclaration();
        if (vtmp && (vtmp.storage_class & STC.rvalue))
        {
            vtmp.storage_class |= STC.nodtor;
        }
    }
    return e;
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
 * Find symbol in accordance with the UFCS name look up rule
 */
private Expression searchUFCS(Scope* sc, UnaExp ue, Identifier ident)
{
    //printf("searchUFCS(ident = %s)\n", ident.toChars());
    Loc loc = ue.loc;

    // TODO: merge with Scope.search.searchScopes()
    Dsymbol searchScopes(SearchOptFlags flags)
    {
        Dsymbol s = null;
        for (Scope* scx = sc; scx; scx = scx.enclosing)
        {
            if (!scx.scopesym)
                continue;
            if (scx.scopesym.isModule())
                flags |= SearchOpt.unqualifiedModule;    // tell Module.search() that SearchOpt.localsOnly is to be obeyed
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

    SearchOptFlags flags = SearchOpt.all;
    Dsymbol s;

    if (sc.ignoresymbolvisibility)
        flags |= SearchOpt.ignoreVisibility;

    // First look in local scopes
    s = searchScopes(flags | SearchOpt.localsOnly);
    if (!s)
    {
        // Second look in imported modules
        s = searchScopes(flags | SearchOpt.importsOnly);
    }

    if (!s)
        return ue.e1.type.getProperty(sc, loc, ident, 0, ue.e1);

    FuncDeclaration f = s.isFuncDeclaration();
    if (f)
    {
        if (TemplateDeclaration td = getFuncTemplateDecl(f))
        {
            if (td.overroot)
                td = td.overroot;
            s = td;
        }
    }

    if (auto dti = ue.isDotTemplateInstanceExp())
    {
        // https://issues.dlang.org/show_bug.cgi?id=23968
        // Typically, deprecated alias declarations are caught
        // when `TemplateInstance.findTempDecl` is called,
        // however, in this case the tempdecl field is updated
        // therefore `findTempDecl` will return immediately
        // and not get the chance to issue the deprecation.
        if (s.isAliasDeclaration())
            s.checkDeprecated(ue.loc, sc);

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
 * check e is exp.opDispatch!(tiargs) or not
 * It's used to switch to UFCS the semantic analysis path
 */
private bool isDotOpDispatch(Expression e)
{
    if (auto dtie = e.isDotTemplateInstanceExp())
        return dtie.ti.name == Id.opDispatch;
    return false;
}

private void hookDtors(CondExp ce, Scope* sc)
{
    extern (C++) final class DtorVisitor : StoppableVisitor
    {
        alias visit = typeof(super).visit;
    public:
        Scope* sc;
        CondExp ce;
        VarDeclaration vcond;
        bool isThen;

        extern (D) this(Scope* sc, CondExp ce) @safe
        {
            this.sc = sc;
            this.ce = ce;
        }

        override void visit(Expression e)
        {
            //printf("(e = %s)\n", e.toChars());
        }

        override void visit(DeclarationExp e)
        {
            auto v = e.declaration.isVarDeclaration();
            if (!v || v.isDataseg())
                return;

            if (v._init)
            {
                if (auto ei = v._init.isExpInitializer())
                    walkPostorder(ei.exp, this);
            }

            if (v.edtor)
                walkPostorder(v.edtor, this);

            if (!v.needsScopeDtor())
                return;

            if (!vcond)
            {
                vcond = copyToTemp(STC.volatile_ | STC.const_, "__cond", ce.econd);
                vcond.dsymbolSemantic(sc);

                Expression de = new DeclarationExp(ce.econd.loc, vcond);
                de = de.expressionSemantic(sc);

                Expression ve = new VarExp(ce.econd.loc, vcond);
                ce.econd = Expression.combine(de, ve);
            }

            //printf("\t++v = %s, v.edtor = %s\n", v.toChars(), v.edtor.toChars());
            Expression ve = new VarExp(vcond.loc, vcond);
            if (isThen)
                v.edtor = new LogicalExp(v.edtor.loc, EXP.andAnd, ve, v.edtor);
            else
                v.edtor = new LogicalExp(v.edtor.loc, EXP.orOr, ve, v.edtor);
            v.edtor = v.edtor.expressionSemantic(sc);
            //printf("\t--v = %s, v.edtor = %s\n", v.toChars(), v.edtor.toChars());
        }
    }

    scope DtorVisitor v = new DtorVisitor(sc, ce);
    //printf("+%s\n", toChars());
    v.isThen = true;
    walkPostorder(ce.e1, v);
    v.isThen = false;
    walkPostorder(ce.e2, v);
    //printf("-%s\n", toChars());
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
                    error(ce.loc, "expected key as argument to `aa.remove()`");
                    return ErrorExp.get();
                }
                if (!eleft.type.isMutable())
                {
                    error(ce.loc, "cannot remove key from `%s` associative array `%s`", MODtoChars(t.mod), eleft.toChars());
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

                    const errors = global.startGagging();
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
        if (Expression ey = dti.dotTemplateSemanticProp(sc, DotExpFlag.gag))
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
    ce.isUfcsRewrite = true;
    return null;
}

int expandAliasThisTuples(Expressions* exps, size_t starti = 0)
{
    if (!exps || exps.length == 0)
        return -1;

    for (size_t u = starti; u < exps.length; u++)
    {
        Expression exp = (*exps)[u];
        if (TupleDeclaration td = exp.isAliasThisTuple)
        {
            exps.remove(u);
            size_t i;
            td.foreachVar((s)
            {
                auto d = s.isDeclaration();
                auto e = new DotVarExp(exp.loc, exp, d);
                assert(d.type);
                e.type = d.type;
                exps.insert(u + i, e);
                ++i;
            });
            version (none)
            {
                printf("expansion ->\n");
                foreach (e; exps)
                {
                    printf("\texps[%d] e = %s %s\n", i, EXPtoString(e.op), e.toChars());
                }
            }
            return cast(int)u;
        }
    }
    return -1;
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
            ex = new AssignExp(loc, ex, e2);
            return ex.expressionSemantic(sc);
        }
        else
        {
            // strict setter prints errors if fails
            e = e.expressionSemantic(sc);
        }
        return e;
    }
    else
    {
        /* f(e1)
         */
        auto arguments = new Expressions(1);
        (*arguments)[0] = eleft;
        e = new CallExp(loc, e, arguments);

        // https://issues.dlang.org/show_bug.cgi?id=24017
        if (sc.debug_)
            e.isCallExp().inDebugStatement = true;

        e = e.expressionSemantic(sc);
        return e;
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
                if (fd.type.isTypeFunction().isProperty)
                    return resolveProperties(sc, e1);
            }
            else if (td && td.onemember && (fd = td.onemember.isFuncDeclaration()) !is null)
            {
                if (fd.type.isTypeFunction().isProperty ||
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
                if (fd.type.isTypeFunction().isProperty ||
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
        if (fd.type.isTypeFunction().isProperty)
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
    if (auto dti = e1.isDotTemplateInstanceExp())
    {
        if (dti.ti.tempdecl)
            if (auto td = dti.ti.tempdecl.isTemplateDeclaration())
                return handleTemplateDecl(td);
    }
    else if (auto dte = e1.isDotTemplateExp())
        return handleTemplateDecl(dte.td);
    if (auto se = e1.isScopeExp())
    {
        Dsymbol s = se.sds;
        TemplateInstance ti = s.isTemplateInstance();
        if (ti && !ti.semanticRun && ti.tempdecl)
            if (auto td = ti.tempdecl.isTemplateDeclaration())
                return handleTemplateDecl(td);
    }
    else if (auto et = e1.isTemplateExp())
        return handleTemplateDecl(et.td);
    if (e1.isDotVarExp() && e1.type.isTypeFunction())
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
                if (sc.setUnsafePreview(sc.previews.systemVariables, false, loc,
                    "access `@system` variable `%s`", sd))
                {
                    if (auto v = sd.isVarDeclaration())
                    {
                        if (v.systemInferred)
                            errorSupplemental(v.loc, "`%s` is inferred to be `@system` from its initializer here", v.toChars());
                        else
                            errorSupplemental(v.loc, "`%s` is declared here", v.toChars());
                    }
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
        if (!functionSemantic(f))
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
        auto cls = ad.isClassDeclaration();
        auto classObj = new ObjcClassReferenceExp(e1.loc, cls);
        classObj.type = objc.getRuntimeMetaclass(cls).getType();
        return classObj;
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
            error(e1.loc, "`this` for `%s` needs to be type `%s` not type `%s`", var.toChars(), ad.toChars(), t.toChars());
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
    // https://issues.dlang.org/show_bug.cgi?id=24013
    // traits(getOverloads) inserts an alias to select the overload.
    // When searching for the right this we need to use the aliased
    // overload/function, not the alias.
    outerFunc = outerFunc.toAliasFunc();
    calledFunc = calledFunc.toAliasFunc();

    auto thisAd = outerFunc.isMemberLocal();
    if (!thisAd)
        return false;

    auto requiredAd = calledFunc.isMemberLocal();
    if (!requiredAd)
        return false;

    if (thisAd == requiredAd)
        return true;

    // if outerfunc is the member of a nested aggregate, then let
    // getRightThis take care of this.
    if (thisAd.isNested())
        return true;

    // outerfunc is the member of a base class that contains calledFunc,
    // then we consider that they have the same this.
    auto cd = requiredAd.isClassDeclaration();
    if (!cd)
        return false;

    if (cd.isBaseOf2(thisAd.isClassDeclaration()))
        return true;

    return false;
}

/*********************************************
 * Calling function f.
 * Check the purity, i.e. if we're in a pure function
 * we can only call other pure functions.
 * Returns true if error occurs.
 */
private bool checkPurity(FuncDeclaration f, const ref Loc loc, Scope* sc)
{
    if (!sc.func)
        return false;
    if (sc.func == f)
        return false;
    if (sc.intypeof == 1)
        return false;
    if (sc.ctfe || sc.debug_)
        return false;

    // If the call has a pure parent, then the called func must be pure.
    if (!f.isPure() && checkImpure(sc, loc, null, f))
    {
        error(loc, "`pure` %s `%s` cannot call impure %s `%s`",
            sc.func.kind(), sc.func.toPrettyChars(), f.kind(),
            f.toPrettyChars());

        if (!f.isDtorDeclaration())
            errorSupplementalInferredAttr(f, /*max depth*/ 10, /*deprecation*/ false, STC.pure_, global.errorSink);

        f.checkOverriddenDtor(sc, loc, dd => dd.type.toTypeFunction().purity != PURE.impure, "impure");
        return true;
    }
    return false;
}

/**
 * Checks whether `f` is a generated `DtorDeclaration` that hides a user-defined one
 * which passes `check` while `f` doesn't (e.g. when the user defined dtor is pure but
 * the generated dtor is not).
 * In that case the method will identify and print all members causing the attribute
 * missmatch.
 *
 * Params:
 *   f  = potential `DtorDeclaration`
 *   sc = scope
 *   loc = location
 *   check = current check (e.g. whether it's pure)
 *   checkName = the kind of check (e.g. `"pure"`)
 */
void checkOverriddenDtor(FuncDeclaration f, Scope* sc, const ref Loc loc,
            scope bool function(DtorDeclaration) check, const string checkName)
{
    auto dd = f.isDtorDeclaration();
    if (!dd || !dd.isGenerated())
        return;

    // DtorDeclaration without parents should fail at an earlier stage
    auto ad = cast(AggregateDeclaration) f.toParent2();
    assert(ad);

    if (ad.userDtors.length)
    {
        if (!check(ad.userDtors[0])) // doesn't match check (e.g. is impure as well)
            return;

        // Sanity check
        assert(!check(ad.fieldDtor));
    }

    dd.loc.errorSupplemental("%s`%s.~this` is %.*s because of the following field's destructors:",
                        dd.isGenerated() ? "generated " : "".ptr,
                        ad.toChars,
                        cast(int) checkName.length, checkName.ptr);

    // Search for the offending fields
    foreach (field; ad.fields)
    {
        // Only structs may define automatically called destructors
        auto ts = field.type.isTypeStruct();
        if (!ts)
        {
            // But they might be part of a static array
            auto ta = field.type.isTypeSArray();
            if (!ta)
                continue;

            ts = ta.baseElemOf().isTypeStruct();
            if (!ts)
                continue;
        }

        auto fieldSym = ts.toDsymbol(sc);
        assert(fieldSym); // Resolving ts must succeed because missing defs. should error before

        auto fieldSd = fieldSym.isStructDeclaration();
        assert(fieldSd); // ts is a TypeStruct, this would imply a malformed ASR

        if (fieldSd.dtor && !check(fieldSd.dtor))
        {
            field.loc.errorSupplemental(" - %s %s", field.type.toChars(), field.toChars());

            if (fieldSd.dtor.isGenerated())
                fieldSd.dtor.checkOverriddenDtor(sc, loc, check, checkName);
            else
                fieldSd.dtor.loc.errorSupplemental("   %.*s `%s.~this` is declared here",
                                        cast(int) checkName.length, checkName.ptr, fieldSd.toChars());
        }
    }
}

/********************************************
 * Print the reason why `fd` was inferred `@system` as a supplemental error
 * Params:
 *   fd = function to check
 *   maxDepth = up to how many functions deep to report errors
 *   deprecation = print deprecations instead of errors
 *   stc = storage class of attribute to check
 *   eSink = where the error messages go
 */
public void errorSupplementalInferredAttr(FuncDeclaration fd, int maxDepth, bool deprecation, STC stc, ErrorSink eSink)
{
    auto errorFunc = deprecation ? &eSink.deprecationSupplemental : &eSink.errorSupplemental;

    AttributeViolation* s;
    string attr;
    if (stc & STC.safe)
    {
        s = fd.safetyViolation;
        attr = "@safe";
    }
    else if (stc & STC.pure_)
    {
        s = fd.pureViolation;
        attr = "pure";
    }
    else if (stc & STC.nothrow_)
    {
        s = fd.nothrowViolation;
        attr = "nothrow";
    }
    else if (stc & STC.nogc)
    {
        s = fd.nogcViolation;
        attr = "@nogc";
    }

    if (!s)
        return;

    if (s.action.length > 0)
    {
        errorFunc(s.loc, "and %.*s makes it fail to infer `%.*s`", s.action.fTuple.expand, attr.fTuple.expand);
    }
    else if (s.fd)
    {
        if (maxDepth > 0)
        {
            errorFunc(s.loc, "which calls `%s`", s.fd.toPrettyChars());
            errorSupplementalInferredAttr(s.fd, maxDepth - 1, deprecation, stc, eSink);
        }
    }
    else
        assert(0);
}

/*******************************************
 * Accessing variable v.
 * Check for purity and safety violations.
 * Returns true if error occurs.
 */
private bool checkPurity(VarDeclaration v, const ref Loc loc, Scope* sc)
{
    //printf("v = %s %s\n", v.type.toChars(), v.toChars());
    /* Look for purity and safety violations when accessing variable v
     * from current function.
     */
    if (!sc.func)
        return false;
    if (sc.intypeof == 1)
        return false; // allow violations inside typeof(expression)
    if (sc.ctfe || sc.debug_)
        return false; // allow violations inside compile-time evaluated expressions and debug conditionals
    if (v.ident == Id.ctfe)
        return false; // magic variable never violates pure and safe
    if (v.isImmutable())
        return false; // always safe and pure to access immutables...
    if (v.isConst() && !v.isReference() && (v.isDataseg() || v.isParameter()) && v.type.implicitConvTo(v.type.immutableOf()))
        return false; // or const global/parameter values which have no mutable indirections
    if (v.storage_class & STC.manifest)
        return false; // ...or manifest constants

    // accessing empty structs is pure
    // https://issues.dlang.org/show_bug.cgi?id=18694
    // https://issues.dlang.org/show_bug.cgi?id=21464
    // https://issues.dlang.org/show_bug.cgi?id=23589
    if (v.type.ty == Tstruct)
    {
        StructDeclaration sd = (cast(TypeStruct)v.type).sym;
        if (sd.members) // not opaque
        {
            if (sd.semanticRun >= PASS.semanticdone)
                sd.determineSize(v.loc);
            if (sd.hasNoFields)
                return false;
        }
    }

    bool err = false;
    if (v.isDataseg())
    {
        // https://issues.dlang.org/show_bug.cgi?id=7533
        // Accessing implicit generated __gate is pure.
        if (v.ident == Id.gate)
            return false;

        if (checkImpure(sc, loc, "accessing mutable static data `%s`", v))
        {
            error(loc, "`pure` %s `%s` cannot access mutable static data `%s`",
                sc.func.kind(), sc.func.toPrettyChars(), v.toChars());
            err = true;
        }
    }
    else
    {
        /* Given:
         * void f() {
         *   int fx;
         *   pure void g() {
         *     int gx;
         *     /+pure+/ void h() {
         *       int hx;
         *       /+pure+/ void i() { }
         *     }
         *   }
         * }
         * i() can modify hx and gx but not fx
         */

        Dsymbol vparent = v.toParent2();
        for (Dsymbol s = sc.func; !err && s; s = s.toParentP(vparent))
        {
            if (s == vparent)
                break;

            if (AggregateDeclaration ad = s.isAggregateDeclaration())
            {
                if (ad.isNested())
                    continue;
                break;
            }
            FuncDeclaration ff = s.isFuncDeclaration();
            if (!ff)
                break;
            if (!ff.isNested() && !ff.isThis())
                break;
            if (ff.type.isImmutable() ||
                ff.type.isShared() && !MODimplicitConv(ff.type.mod, v.type.mod))
            {
                OutBuffer ffbuf;
                OutBuffer vbuf;
                MODMatchToBuffer(&ffbuf, ff.type.mod, v.type.mod);
                MODMatchToBuffer(&vbuf, v.type.mod, ff.type.mod);
                error(loc, "%s%s `%s` cannot access %sdata `%s`",
                    ffbuf.peekChars(), ff.kind(), ff.toPrettyChars(), vbuf.peekChars(), v.toChars());
                err = true;
                break;
            }
            continue;
        }
    }

    /* Do not allow safe functions to access __gshared data
     */
    if (v.storage_class & STC.gshared)
    {
        if (sc.setUnsafe(false, loc, "accessing `__gshared` data `%s`", v))
        {
            err = true;
        }
    }

    return err;
}

/*
Check if sc.func is impure or can be made impure.
Returns true on error, i.e. if sc.func is pure and cannot be made impure.
*/
private bool checkImpure(Scope* sc, Loc loc, const(char)* fmt, RootObject arg0)
{
    return sc.func && (isRootTraitsCompilesScope(sc)
            ? sc.func.isPureBypassingInference() >= PURE.weak
            : sc.func.setImpure(loc, fmt, arg0));
}

/*********************************************
 * Calling function f.
 * Check the safety, i.e. if we're in a @safe function
 * we can only call @safe or @trusted functions.
 * Returns true if error occurs.
 */
private bool checkSafety(FuncDeclaration f, ref Loc loc, Scope* sc)
{
    if (sc.func == f)
        return false;
    if (sc.intypeof == 1)
        return false;
    if (sc.debug_)
        return false;
    if (sc.ctfe && sc.func)
        return false;

    if (!sc.func)
    {
        if (sc.varDecl && !f.safetyInprocess && !f.isSafe() && !f.isTrusted())
        {
            if (sc.varDecl.storage_class & STC.safe)
            {
                error(loc, "`@safe` variable `%s` cannot be initialized by calling `@system` function `%s`",
                    sc.varDecl.toChars(), f.toChars());
                return true;
            }
            else
            {
                sc.varDecl.storage_class |= STC.system;
                sc.varDecl.systemInferred = true;
            }
        }
        return false;
    }

    if (!f.isSafe() && !f.isTrusted())
    {
        if (isRootTraitsCompilesScope(sc) ? sc.func.isSafeBypassingInference() : sc.func.setUnsafeCall(f))
        {
            if (!loc.isValid()) // e.g. implicitly generated dtor
                loc = sc.func.loc;

            const prettyChars = f.toPrettyChars();
            error(loc, "`@safe` %s `%s` cannot call `@system` %s `%s`",
                sc.func.kind(), sc.func.toPrettyChars(), f.kind(),
                prettyChars);
            if (!f.isDtorDeclaration)
                errorSupplementalInferredAttr(f, /*max depth*/ 10, /*deprecation*/ false, STC.safe, global.errorSink);
            .errorSupplemental(f.loc, "`%s` is declared here", prettyChars);

            f.checkOverriddenDtor(sc, loc, dd => dd.type.toTypeFunction().trust > TRUST.system, "@system");

            return true;
        }
    }
    else if (f.isSafe() && f.safetyViolation)
    {
        // for dip1000 by default transition, print deprecations for calling functions that will become `@system`
        if (sc.func.isSafeBypassingInference())
        {
            .deprecation(loc, "`@safe` function `%s` calling `%s`", sc.func.toChars(), f.toChars());
            errorSupplementalInferredAttr(f, 10, true, STC.safe, global.errorSink);
        }
        else if (!sc.func.safetyViolation)
        {
            import dmd.func : AttributeViolation;
            sc.func.safetyViolation = new AttributeViolation(loc, f);
        }
    }
    return false;
}

/*********************************************
 * Calling function f.
 * Check the @nogc-ness, i.e. if we're in a @nogc function
 * we can only call other @nogc functions.
 * Returns true if error occurs.
 */
private bool checkNogc(FuncDeclaration f, ref Loc loc, Scope* sc)
{
    if (!sc.func)
        return false;
    if (sc.func == f)
        return false;
    if (sc.intypeof == 1)
        return false;
    if (sc.ctfe || sc.debug_)
        return false;
    /* The original expressions (`new S(...)` or `new S[...]``) will be
     * verified instead. This is to keep errors related to the original code
     * and not the lowering.
     */
    if (f.ident == Id._d_newitemT || f.ident == Id._d_newarrayT || f.ident == Id._d_newarraymTX)
        return false;

    if (f.isNogc())
        return false;

    if (isRootTraitsCompilesScope(sc) ? !sc.func.isNogcBypassingInference() : !sc.func.setGCCall(f))
        return false;

    if (loc.linnum == 0) // e.g. implicitly generated dtor
        loc = sc.func.loc;

    // Lowered non-@nogc'd hooks will print their own error message inside of nogc.d (NOGCVisitor.visit(CallExp e)),
    // so don't print anything to avoid double error messages.
    if (!(f.ident == Id._d_HookTraceImpl || f.ident == Id._d_arraysetlengthT
        || f.ident == Id._d_arrayappendT || f.ident == Id._d_arrayappendcTX
        || f.ident == Id._d_arraycatnTX || f.ident == Id._d_newclassT))
    {
        error(loc, "`@nogc` %s `%s` cannot call non-@nogc %s `%s`",
            sc.func.kind(), sc.func.toPrettyChars(), f.kind(), f.toPrettyChars());

        if (!f.isDtorDeclaration)
            f.errorSupplementalInferredAttr(/*max depth*/ 10, /*deprecation*/ false, STC.nogc, global.errorSink);
    }

    f.checkOverriddenDtor(sc, loc, dd => dd.type.toTypeFunction().isNogc, "non-@nogc");

    return true;
}

/********************************************
 * Check that the postblit is callable if t is an array of structs.
 * Returns true if error happens.
 */
private bool checkPostblit(Type t, ref Loc loc, Scope* sc)
{
    auto ts = t.baseElemOf().isTypeStruct();
    if (!ts)
        return false;

    if (global.params.useTypeInfo && Type.dtypeinfo)
    {
        // https://issues.dlang.org/show_bug.cgi?id=11395
        // Require TypeInfo generation for array concatenation
        semanticTypeInfo(sc, t);
    }

    StructDeclaration sd = ts.sym;
    if (!sd.postblit)
        return false;

    if (sd.postblit.checkDisabled(loc, sc))
        return true;

    //checkDeprecated(sc, sd.postblit);        // necessary?
    sd.postblit.checkPurity(loc, sc);
    sd.postblit.checkSafety(loc, sc);
    sd.postblit.checkNogc(loc, sc);
    //checkAccess(sd, loc, sc, sd.postblit);   // necessary?
    return false;

}

/***************************************
 * Pull out any properties.
 */
private Expression resolvePropertiesX(Scope* sc, Expression e1, Expression e2 = null, BinExp saveAtts = null)
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

            Expressions* a = new Expressions();
            a.push(e2);

            for (size_t i = 0; i < os.a.length; i++)
            {
                if (FuncDeclaration f = resolveFuncCall(loc, sc, os.a[i], tiargs, tthis, ArgumentList(a), FuncResolveFlag.quiet))
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
        for (size_t i = 0; i < os.a.length; i++)
        {
            FuncDeclaration f = resolveFuncCall(loc, sc, os.a[i], tiargs, tthis, ArgumentList(), FuncResolveFlag.quiet);
            if (!f)
                continue;
            if (f.errors)
                return ErrorExp.get();
            fd = f;
            assert(fd.type.ty == Tfunction);
            auto tf = fd.type.isTypeFunction();
            if (!tf.isRef && e2)
            {
                error(loc, "%s is not an lvalue", e1.toChars());
                return ErrorExp.get();
            }
        }
        if (fd)
        {
            Expression e = new CallExp(loc, e1);
            if (e2)
            {
                e = new AssignExp(loc, e, e2);
                if (saveAtts)
                {
                    (cast(BinExp)e).att1 = saveAtts.att1;
                    (cast(BinExp)e).att2 = saveAtts.att2;
                }
            }
            return e.expressionSemantic(sc);
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
    else if (sc && sc.inCfile && e1.isVarExp() && !e2)
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

            Expressions* a = new Expressions();
            a.push(e2);

            FuncDeclaration fd = resolveFuncCall(loc, sc, s, tiargs, tthis, ArgumentList(a), FuncResolveFlag.quiet);
            if (fd && fd.type)
            {
                if (fd.errors)
                    return ErrorExp.get();
                assert(fd.type.ty == Tfunction);
                Expression e = new CallExp(loc, e1, e2);
                return e.expressionSemantic(sc);
            }
        }
        FuncDeclaration fd = resolveFuncCall(loc, sc, s, tiargs, tthis, ArgumentList(), FuncResolveFlag.quiet);
        if (fd && fd.type)
        {
            if (fd.errors)
                return ErrorExp.get();
            TypeFunction tf = fd.type.isTypeFunction();
            if (!e2 || tf.isRef)
            {
                Expression e = new CallExp(loc, e1);
                if (e2)
                {
                    e = new AssignExp(loc, e, e2);
                    if (saveAtts)
                    {
                        (cast(BinExp)e).att1 = saveAtts.att1;
                        (cast(BinExp)e).att2 = saveAtts.att2;
                    }
                }
                return e.expressionSemantic(sc);
            }
        }
        if (FuncDeclaration fd2 = s.isFuncDeclaration())
        {
            // Keep better diagnostic message for invalid property usage of functions
            assert(fd2.type.ty == Tfunction);
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
            if (v.checkPurity(ve.loc, sc))
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

private bool checkRightThis(Expression e, Scope* sc)
{
    if (e.op == EXP.error)
        return true;
    if (e.op != EXP.variable || e.type.ty == Terror)
        return false;

    VarExp ve = cast(VarExp)e;
    if (!isNeedThisScope(sc, ve.var))
        return false;

    //printf("checkRightThis sc.intypeof = %d, ad = %p, func = %p, fdthis = %p\n",
    //        sc.intypeof, sc.getStructClassScope(), func, fdthis);
    auto t = ve.var.isThis();
    assert(t);
    error(e.loc, "accessing non-static variable `%s` requires an instance of `%s`", ve.var.toChars(), t.toChars());
    return true;
}

Expression resolveProperties(Scope* sc, Expression e)
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
            error(e.loc, "`%s` has no value", e.toChars());
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

        e = doCopyOrMove(sc, e, null, false);

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

private Expression opAssignToOp(const ref Loc loc, EXP op, Expression e1, Expression e2) @safe
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
 *     eSink        =  if not null, used to report errors. Some callers are not
 *                      checking actual function params, so they'll do their own error reporting
 * Returns:
 *     `true` when a semantic error occurred
 */
private bool preFunctionParameters(Scope* sc, ArgumentList argumentList, ErrorSink eSink)
{
    Expressions* exps = argumentList.arguments;
    if (!exps)
        return false;

    expandTuples(exps, argumentList.names);

    bool err = false;
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
                if (eSink)
                {
                    eSink.error(arg.loc, "cannot pass type `%s` as a function argument", arg.toChars());
                    arg = ErrorExp.get();
                }
                err = true;
            }
        }
        else if (arg.type.toBasetype().ty == Tfunction)
        {
            if (eSink)
            {
                eSink.error(arg.loc, "cannot pass function `%s` as a function argument", arg.toChars());
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
            .error(loc, "%s `%s` default construction is disabled", sd.kind, sd.toPrettyChars);
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
    //printf("functionParameters() fd: %s tf: %s\n", fd ? fd.ident.toChars() : "", toChars(tf));
    assert(arguments);
    assert(fd || tf.next);
    const size_t nparams = tf.parameterList.length;
    const olderrors = global.errors;
    bool err = false;
    Expression eprefix = null;
    *peprefix = null;

    if (argumentList.names)
    {
        OutBuffer buf;
        auto resolvedArgs = tf.resolveNamedArgs(argumentList, &buf);
        if (!resolvedArgs)
        {
            // while errors are usually already caught by `tf.callMatch`,
            // this can happen when calling `typeof(freefunc)`
            if (buf.length)
                error(loc, "%s", buf.peekChars());
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
        functionSemantic(fd);
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

        if (i >= nparams)
            break;

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
        else if (arg.isDefaultInitExp())
        {
            arg = arg.resolveLoc(loc, sc);
            (*arguments)[i] = arg;
        }

        if (tf.parameterList.varargs == VarArg.typesafe && i + 1 == nparams) // https://dlang.org/spec/function.html#variadic
        {
            //printf("\t\tvarargs == 2, p.type = '%s'\n", p.type.toChars());
            if (MATCH m = arg.implicitConvTo(p.type))
            {
                if (p.type.nextOf() && arg.implicitConvTo(p.type.nextOf()) >= m)
                    goto L2;
                else if (nargs != nparams)
                    return errorArgs();
                goto L1;
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
                        assert(a);
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

        if (done)
            break;
    }
    if ((wildmatch == MODFlags.mutable || wildmatch == MODFlags.immutable_) &&
        tf.next && tf.next.hasWild() &&
        (tf.isRef || !tf.next.implicitConvTo(tf.next.immutableOf())))
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
                arg = arg.toLvalue(sc, "create `in` parameter from");

                // Look for mutable misaligned pointer, etc., in @safe mode
                err |= checkUnsafeAccess(sc, arg, false, true);
            }
            else if (p.storageClass & STC.ref_)
            {
                if (sc.previews.rvalueRefParam &&
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
                arg = arg.toLvalue(sc, "create `ref` parameter from");

                // Look for mutable misaligned pointer, etc., in @safe mode
                err |= checkUnsafeAccess(sc, arg, false, true);
            }
            else if (p.storageClass & STC.out_)
            {
                Type t = arg.type;
                if (!t.isMutable() || !t.isAssignable()) // check blit assignable
                {
                    error(arg.loc, "cannot modify struct `%s` with immutable members", arg.toChars());
                    err = true;
                }
                else
                {
                    // Look for misaligned pointer, etc., in @safe mode
                    err |= checkUnsafeAccess(sc, arg, false, true);
                    err |= checkDefCtor(arg.loc, t); // t must be default constructible
                }
                arg = arg.toLvalue(sc, "create `out` parameter from");
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

            const indirect = (fd is null) || (fd.isVirtual() && !fd.isFinal());
            const pStc = tf.parameterStorageClass(tthis, p, fd ? &fd.outerVars : null, indirect);

            if (firstArg && (pStc & STC.return_))
            {
                /* Argument value can be assigned to firstArg.
                 * Check arg to see if it matters.
                 */
                err |= checkParamArgumentReturn(*sc, firstArg, arg, p, false);
            }
            // Allow 'lazy' to imply 'scope' - lazy parameters can be passed along
            // as lazy parameters to the next function, but that isn't escaping.
            // The arguments of `_d_arraycatnTX` are already handled in
            // expressionsem.d, via `checkNewEscape`. Without `-dip1000`, the
            // check does not return an error, so the lowering of `a ~ b` to
            // `_d_arraycatnTX(a, b)` still occurs.
            else if (!(pStc & STC.lazy_) && (!fd ||  fd.ident != Id._d_arraycatnTX))
            {
                /* Argument value can escape from the called function.
                 * Check arg to see if it matters.
                 */
                VarDeclaration vPar = fd ? (fd.parameters ? (*fd.parameters)[i] : null) : null;
                err |= checkParamArgumentEscape(*sc, fd, p.ident, vPar, cast(STC) pStc, arg, false, false);
            }

            // Turning heap allocations into stack allocations is dangerous without dip1000, since `scope` inference
            // may be unreliable when scope violations only manifest as deprecation warnings.
            // However, existing `@nogc` code may rely on it, so still do it when the parameter is explicitly marked `scope`
            const explicitScope = p.isLazy() ||
                ((p.storageClass & STC.scope_) && !(p.storageClass & STC.scopeinferred));
            if ((pStc & (STC.scope_ | STC.lazy_)) &&
                ((sc.useDIP1000 == FeatureState.enabled) || explicitScope) &&
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
                    tmp.storage_class |= STC.exptemp;
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
                if (tf.parameterList.varargs == VarArg.variadic ||
                    tf.parameterList.varargs == VarArg.KRvariadic)
                {
                    const(char)* p = tf.linkage == LINK.c ? "extern(C)" : "extern(C++)";
                    if (arg.type.ty == Tarray)
                    {
                        error(arg.loc, "cannot pass dynamic arrays to `%s` vararg functions", p);
                        err = true;
                    }
                    if (arg.type.ty == Tsarray)
                    {
                        error(arg.loc, "cannot pass static arrays to `%s` vararg functions", p);
                        err = true;
                    }
                }
            }

            // Do not allow types that need destructors or copy constructors.
            if (arg.type.needsDestruction())
            {
                error(arg.loc, "cannot pass types that need destruction as variadic arguments");
                err = true;
            }
            if (arg.type.needsCopyOrPostblit())
            {
                error(arg.loc, "cannot pass types with postblits or copy constructors as variadic arguments");
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
                    error(arg.loc, "function `%s` is overloaded", arg.toChars());
                    err = true;
                }
            }
            err |= arg.checkValue();
            err |= arg.checkSharedAccess(sc);
            err |= checkParamArgumentEscape(*sc, fd, Id.dotdotdot, null, cast(STC) tf.parameterList.stc, arg, false, false);
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
            checkPrintfFormat(se.loc, se.peekString(), (*arguments)[nparams .. nargs], isVa_list, sc.eSink);
        }
    }
    else if (fd && fd.scanf)
    {
        if (auto se = (*arguments)[nparams - 1 - isVa_list].isStringExp())
        {
            checkScanfFormat(se.loc, se.peekString(), (*arguments)[nparams .. nargs], isVa_list, sc.eSink);
        }
    }
    else
    {
        // TODO: not checking the "v" functions yet (for those, check format string only, not args)
    }

    /* Remaining problems:
     * 1. value structs (or static arrays of them) that need to be copy constructed
     * 2. value structs (or static arrays of them) that have destructors, and subsequent arguments that may throw before the
     *    function gets called.
     * 3. value structs need to be destructed after the function call for platforms where the caller destroys the arguments.
     * Those are handled by doing the argument construction in 'eprefix' so that if a later argument throws, they are cleaned
     * up properly. Pushing arguments on the stack then cannot fail.
     */
     {
        /* Does Problem (3) apply?
         */
        const bool callerDestroysArgs = !target.isCalleeDestroyingArgs(tf);

        /* Compute indices of last throwing argument and first arg needing destruction.
         * Used to not set up destructors unless an arg needs destruction on a throw
         * in a later argument.
         */
        ptrdiff_t lastthrow = -1;   // last argument that may throw
        ptrdiff_t firstdtor = -1;   // first argument that needs destruction
        ptrdiff_t lastdtor  = -1;   // last argument that needs destruction
        for (ptrdiff_t i = 0; i != nargs; i++)
        {
            Expression arg = (*arguments)[i];
            if (canThrow(arg, sc.func, null))
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

        /* Do we need 'eprefix' for problems 2 or 3?
         */
        const bool needsPrefix = callerDestroysArgs
            ? firstdtor >= 0 // true if any argument needs destruction
            : firstdtor >= 0 && lastthrow >= 0 &&
              (lastthrow - firstdtor) > 0; // last throw after first destruction
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

        foreach (ptrdiff_t i; 0 .. nargs)
        {
            Expression arg = (*arguments)[i];
            //printf("arg[%d]: %s\n", cast(int)i, arg.toChars());

            Parameter parameter = i < nparams ? tf.parameterList[i] : null;
            const bool isRef = parameter && parameter.isReference();
            const bool isLazy = parameter && parameter.isLazy();

            /* Skip lazy parameters
             */
            if (isLazy)
                continue;

            /* Do we have 'eprefix' and aren't past 'lastPrefix' yet?
             * Then declare a temporary variable for this arg and append that declaration
             * to 'eprefix', which will implicitly take care of potential problem 1) for
             * this arg.
             * 'eprefix' will therefore finally contain all args up to and including 'lastPrefix',
             * excluding all lazy parameters.
             */
            if (needsPrefix && i <= lastPrefix)
            {
                const bool needsDtor = !isRef && arg.type.needsDestruction() &&
                                       // Problem 3: last throwing arg doesn't require dtor patching
                                       (callerDestroysArgs || i != lastPrefix);

                /* Declare temporary 'auto __pfx = arg' (needsDtor) or 'auto __pfy = arg' (!needsDtor)
                 */
                auto tmp = copyToTemp(
                    (parameter ? parameter.storageClass : tf.parameterList.stc) & (STC.return_ | STC.scope_),
                    needsDtor ? "__pfx" : "__pfy",
                    isRef ? arg.addressOf() : arg);
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
                    /* Problem 2: Modify the destructor so it only runs if gate==false,
                     * i.e., only if there was a throw while constructing the args
                     */
                    if (needsDtor)
                    {
                        // edtor => (__gate || edtor)
                        assert(tmp.edtor);
                        Expression e = tmp.edtor;
                        e = new LogicalExp(e.loc, EXP.orOr, new VarExp(e.loc, gate), e);
                        tmp.edtor = e.expressionSemantic(sc);
                        //printf("edtor: %s\n", tmp.edtor.toChars());
                    }
                    else
                    {
                        if (tmp.edtor)
                        {
                            assert(i == lastPrefix);
                            tmp.edtor = null;
                        }
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

                /* Problem 2: Last throwing arg?
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
                /* Handle problem 1) by calling the copy constructor for value structs
                 * (or static arrays of them) if appropriate.
                 */
                Type tv = arg.type.baseElemOf();
                if (!isRef && tv.ty == Tstruct)
                    arg = doCopyOrMove(sc, arg, parameter ? parameter.type : null, false);
            }

            (*arguments)[i] = arg;
        }
    }
    //if (eprefix) printf("eprefix: %s\n", eprefix.toChars());

    /* Test compliance with DIP1021 Argument Ownership and Function Calls
     */
    if (sc.previews.dip1021 && (tf.trust == TRUST.safe || tf.trust == TRUST.default_) ||
        tf.isLive)
        err |= checkMutableArguments(*sc, fd, tf, ethis, arguments, false);

    // If D linkage and variadic, add _arguments[] as first argument
    if (tf.isDstyleVariadic())
    {
        assert(arguments.length >= nparams);

        auto args = new Parameters(arguments.length - nparams);
        for (size_t i = 0; i < arguments.length - nparams; i++)
        {
            Expression earg = (*arguments)[nparams + i];
            auto arg = new Parameter(earg.loc, STC.in_, earg.type, null, null, null);
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

    this(Scope* sc) scope @safe
    {
        this.sc = sc;
    }

    private void setError()
    {
        result = ErrorExp.get();
    }

    private void needThisError(Loc loc, FuncDeclaration f)
    {
        auto t = f.isThis();
        assert(t);
        .error(loc, "calling non-static function `%s` requires an instance of type `%s`", f.toChars(), t.toChars());
        setError();
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
        else if (!e.type.isImaginary || !sc.inCfile)
        {
            e.type = e.type.typeSemantic(e.loc, sc);
            result = e;
            return;
        }

        /* Convert to core.stdc.config.complex
         */
        Type t = getComplexLibraryType(e.loc, sc, e.type.ty);
        if (t.ty == Terror)
            return setError();

        Type tf;
        switch (e.type.ty)
        {
            case Timaginary32: tf = Type.tfloat32; break;
            case Timaginary64: tf = Type.tfloat64; break;
            case Timaginary80: tf = Type.tfloat80; break;
            default:
                assert(0);
        }

        /* Construct ts{re : 0.0, im : e}
         */
        TypeStruct ts = t.isTypeStruct;
        Expressions* elements = new Expressions(2);
        (*elements)[0] = new RealExp(e.loc,    CTFloat.zero, tf);
        (*elements)[1] = new RealExp(e.loc, e.toImaginary(), tf);
        Expression sle = new StructLiteralExp(e.loc, ts.sym, elements);
        result = sle.expressionSemantic(sc);
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

        scope (exit) result.rvalue = exp.rvalue;

        Dsymbol scopesym;
        Dsymbol s = sc.search(exp.loc, exp.ident, scopesym);
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
                        error(exp.loc, "with symbol `%s` is shadowing local symbol `%s`", s.toPrettyChars(), s2.toPrettyChars());
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

                if (sc.previews.fixAliasThis)
                {
                    if (ExpressionDsymbol expDsym = scopesym.isExpressionDsymbol())
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

        if (!sc.previews.fixAliasThis && hasThis(sc))
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
            if (sc.ctfe)
            {
                error(exp.loc, "variable `__ctfe` cannot be read at compile time");
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

            auto ss = sc2.scopesym.isWithScopeSymbol();
            if (!ss)
                continue;

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
                Type t = ss.withstate.exp.isTypeExp().type;
                if (!t)
                    continue;

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

        /* Look for what user might have meant
         */
        if (const n = importHint(exp.ident.toString()))
            error(exp.loc, "`%s` is not defined, perhaps `import %.*s;` is needed?", exp.ident.toChars(), cast(int)n.length, n.ptr);
        else if (auto s2 = sc.search_correct(exp.ident))
            error(exp.loc, "undefined identifier `%s`, did you mean %s `%s`?", exp.ident.toChars(), s2.kind(), s2.toChars());
        else if (const p = Scope.search_correct_C(exp.ident))
            error(exp.loc, "undefined identifier `%s`, did you mean `%s`?", exp.ident.toChars(), p);
        else if (exp.ident == Id.dollar)
            error(exp.loc, "undefined identifier `$`");
        else
            error(exp.loc, "undefined identifier `%s`", exp.ident.toChars());

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
                    error(e.loc, "`%s` is not in a class or struct scope", e.toChars());
                    return setError();
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
        {
            error(e.loc, "`this` is only defined in non-static member functions, not `%s`", sc.parent.toChars());
            return setError();
        }

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
    }

    override void visit(SuperExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("SuperExp::semantic('%s')\n", e.toChars());
        }

        FuncDeclaration fd = hasThis(sc);
        ClassDeclaration cd;
        Dsymbol s;

        void err()
        {
            error(e.loc, "`super` is only allowed in non-static class member functions");
            result = ErrorExp.get();
        }
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
                    error(e.loc, "`%s` is not in a class scope", e.toChars());
                    return setError();
                }
                cd = s.isClassDeclaration();
                if (!cd)
                    continue;

                cd = cd.baseClass;
                if (!cd)
                {
                    error(e.loc, "class `%s` has no `super`", s.toChars());
                    return setError();
                }
                e.type = cd.type;
                result = e;
                return;
            }
        }
        if (!fd)
            return err();

        e.var = fd.vthis;
        assert(e.var && e.var.parent);

        s = fd.toParentDecl();
        if (s.isTemplateDeclaration()) // allow inside template constraint
            s = s.toParent();
        assert(s);
        cd = s.isClassDeclaration();
        //printf("parent is %s %s\n", fd.toParent().kind(), fd.toParent().toChars());
        if (!cd)
            return err();
        if (!cd.baseClass)
        {
            error(e.loc, "no base class for `%s`", cd.toChars());
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
    }

    override void visit(NullExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("NullExp::semantic('%s')\n", e.toChars());
        }
        // NULL is the same as (void *)0
        e.type = Type.tnull;
        result = e;
    }

    override void visit(InterpExp e)
    {
        // the lexer breaks up into an odd/even array of literals and expression code
        // we need to turn that into:
        /+
            tuple(
                .object.imported!"core.interpolation".InterpolationHeader(),
                ...
                .object.imported!"core.interpolation".InterpolationFooter()
            )

            There the ... loops through them all, making the even ones
                .object.imported!"core.interpolation".InterpolatedLiteral!str()
            and making the odd ones
                .object.imported!"core.interpolation".InterpolatedExpression!str(),
                the code represented by str

            Empty string literals are skipped as they provide no additional information.
        +/

        if (e.postfix)
            error(e.loc, "String postfixes on interpolated expression sequences are not allowed.");

        Expression makeNonTemplateItem(Identifier which) {
            Expression id = new IdentifierExp(e.loc, Id.empty);
            id = new DotIdExp(e.loc, id, Id.object);
            auto moduleNameArgs = new Objects();
            moduleNameArgs.push(new StringExp(e.loc, "core.interpolation"));
            id = new DotTemplateInstanceExp(e.loc, id, Id.imported, moduleNameArgs);
            id = new DotIdExp(e.loc, id, which);
            id = new CallExp(e.loc, id, new Expressions());
            return id;
        }

        Expression makeTemplateItem(Identifier which, string arg) {
            Expression id = new IdentifierExp(e.loc, Id.empty);
            id = new DotIdExp(e.loc, id, Id.object);
            auto moduleNameArgs = new Objects();
            moduleNameArgs.push(new StringExp(e.loc, "core.interpolation"));
            id = new DotTemplateInstanceExp(e.loc, id, Id.imported, moduleNameArgs);
            auto tiargs = new Objects();
            auto templateStringArg = new StringExp(e.loc, arg);
            // banning those instead of forwarding them
            // templateStringArg.postfix = e.postfix; // forward the postfix to these literals
            tiargs.push(templateStringArg);
            id = new DotTemplateInstanceExp(e.loc, id, which, tiargs);
            id = new CallExp(e.loc, id, new Expressions());
            return id;
        }

        auto arguments = new Expressions();
        arguments.push(makeNonTemplateItem(Id.InterpolationHeader));

        foreach (idx, str; e.interpolatedSet.parts)
        {
            if (idx % 2 == 0)
            {
                if (str.length > 0)
                    arguments.push(makeTemplateItem(Id.InterpolatedLiteral, str));
            }
            else
            {
                arguments.push(makeTemplateItem(Id.InterpolatedExpression, str));
                Expressions* mix = new Expressions();
                mix.push(new StringExp(e.loc, str));
                // FIXME: i'd rather not use MixinExp but idk how to do it lol
                arguments.push(new MixinExp(e.loc, mix));
            }
        }

        arguments.push(makeNonTemplateItem(Id.InterpolationFooter));

        auto loweredTo = new TupleExp(e.loc, arguments);
        visit(loweredTo);

        result = loweredTo;
    }

    override void visit(StringExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("StringExp::semantic() %s\n", e.toChars());
        }

        OutBuffer buffer;
        size_t newlen = 0;
        size_t u;
        dchar c;

        if (e.hexString)
        {
            switch (e.postfix)
            {
                case 'd':
                    e.committed = true;
                    e.sz = 4;
                    e.type = Type.tdstring;
                    break;
                case 'w':
                    e.committed = true;
                    e.sz = 2;
                    e.type = Type.twstring;
                    break;
                case 'c':
                    e.committed = true;
                    goto default;
                default:
                    e.type = Type.tstring;
                    e.sz = 1;
                    break;
            }
            if ((e.len % e.sz) != 0)
                error(e.loc, "hex string with `%s` type needs to be multiple of %d bytes, not %d",
                    e.type.toChars(), e.sz, cast(int) e.len);

            e.setData(arrayCastBigEndian(e.peekData(), e.sz).ptr, e.len / e.sz, e.sz);
        }
        else switch (e.postfix)
        {
        case 'd':
            for (u = 0; u < e.len;)
            {
                if (const p = utf_decodeChar(e.peekString(), u, c))
                {
                    error(e.loc, "%.*s", cast(int)p.length, p.ptr);
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
            if (sc && sc.inCfile)
                e.type = Type.tuns32.sarrayOf(e.len + 1);
            else
                e.type = Type.tdchar.immutableOf().arrayOf();
            e.committed = true;
            break;

        case 'w':
            for (u = 0; u < e.len;)
            {
                if (const p = utf_decodeChar(e.peekString(), u, c))
                {
                    error(e.loc, "%.*s", cast(int)p.length, p.ptr);
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
            if (sc && sc.inCfile)
                e.type = Type.tuns16.sarrayOf(e.len + 1);
            else
                e.type = Type.twchar.immutableOf().arrayOf();
            e.committed = true;
            break;

        case 'c':
            e.committed = true;
            goto default;

        default:
            if (sc && sc.inCfile)
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
                error(exp.loc, "`%s` has no value", e.toChars());
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
            error(e.loc, "`%s` of type `%s` has no value", e.toChars(), e.type.toChars());
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

        // Run semantic() on each element
        bool err_keys = arrayExpressionSemantic(e.keys.peekSlice(), sc);
        bool err_vals = arrayExpressionSemantic(e.values.peekSlice(), sc);
        if (err_keys || err_vals)
            return setError();

        expandTuples(e.keys);
        expandTuples(e.values);
        if (e.keys.length != e.values.length)
        {
            error(e.loc, "number of keys is %llu, must match number of values %llu",
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

        if (checkAssocArrayLiteralEscape(*sc, e, false))
            return setError();

        result = e;
    }

    override void visit(StructLiteralExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("StructLiteralExp::semantic('%s')\n", e.toChars());
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
        auto e = initializerToExpression(init, t, sc.inCfile);
        if (!e)
        {
            error(cle.loc, "cannot convert initializer `%s` to expression", toChars(init));
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
                // printf("apply fix for bugzilla issue 9490: add `this.` to `%s`...\n", e.toChars());
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
                    error(exp.loc, "forward reference of %s `%s`", v.kind(), v.toChars());
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
                        error(exp.loc, "recursive expansion of %s `%s`", ti.kind(), ti.toPrettyChars());
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

    /**
     * Sets the `lowering` field of a `NewExp` to a call to `_d_newitemT` unless
     * compiling with `-betterC` or within `__traits(compiles)`.
     *
     * Params:
     *  ne = the `NewExp` to lower
     */
    private void tryLowerToNewItem(NewExp ne)
    {
        if (!global.params.useGC || !sc.needsCodegen())
            return;

        auto hook = global.params.tracegc ? Id._d_newitemTTrace : Id._d_newitemT;
        if (!verifyHookExist(ne.loc, *sc, hook, "new struct"))
            return;

        /* Lower the memory allocation and initialization of `new T()` to
         * `_d_newitemT!T()`.
         */
        Expression id = new IdentifierExp(ne.loc, Id.empty);
        id = new DotIdExp(ne.loc, id, Id.object);
        auto tiargs = new Objects();
        /*
         * Remove `inout`, `const`, `immutable` and `shared` to reduce the
         * number of generated `_d_newitemT` instances.
         */
        auto t = ne.type.nextOf.unqualify(MODFlags.wild | MODFlags.const_ |
            MODFlags.immutable_ | MODFlags.shared_);
        tiargs.push(t);
        id = new DotTemplateInstanceExp(ne.loc, id, hook, tiargs);

        auto arguments = new Expressions();
        if (global.params.tracegc)
        {
            auto funcname = (sc.callsc && sc.callsc.func) ?
                sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
            arguments.push(new StringExp(ne.loc, ne.loc.filename.toDString()));
            arguments.push(new IntegerExp(ne.loc, ne.loc.linnum, Type.tint32));
            arguments.push(new StringExp(ne.loc, funcname.toDString()));
        }
        id = new CallExp(ne.loc, id, arguments);

        ne.lowering = id.expressionSemantic(sc);
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
                error(exp.loc, "`this` for nested class must be a class type, not `%s`", exp.thisexp.type.toChars());
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
        if (preFunctionParameters(sc, exp.argumentList, global.errorSink))
        {
            return setError();
        }

        if (exp.thisexp && tb.ty != Tclass)
        {
            error(exp.loc, "`.new` is only for allocating nested classes, not `%s`", tb.toChars());
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
                error(exp.loc, "default construction is disabled for type `%s`", cd.type.toChars());
                return setError();
            }

            if (cd.isInterfaceDeclaration())
            {
                error(exp.loc, "cannot create instance of interface `%s`", cd.toChars());
                return setError();
            }

            if (cd.isAbstract())
            {
                error(exp.loc, "cannot create instance of abstract class `%s`", cd.toChars());
                errorSupplemental(cd.loc, "class `%s` is declared here", cd.toChars());
                for (size_t i = 0; i < cd.vtbl.length; i++)
                {
                    FuncDeclaration fd = cd.vtbl[i].isFuncDeclaration();
                    if (fd && fd.isAbstract())
                    {
                        errorSupplemental(fd.loc, "function `%s` is not implemented",
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
                                error(exp.loc, "cannot construct anonymous nested class because no implicit `this` reference to outer class is available");
                            else
                                error(exp.loc, "cannot construct nested class `%s` because no implicit `this` reference to outer class `%s` is available",
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
                        error(exp.loc, "`this` for nested class must be of type `%s`, not `%s`",
                            cdn.toChars(), exp.thisexp.type.toChars());
                        return setError();
                    }
                    if (!MODimplicitConv(exp.thisexp.type.mod, exp.newtype.mod))
                    {
                        error(exp.loc, "nested type `%s` should have the same or weaker constancy as enclosing type `%s`",
                            exp.newtype.toChars(), exp.thisexp.type.toChars());
                        return setError();
                    }
                }
                else if (exp.thisexp)
                {
                    error(exp.loc, "`.new` is only for allocating nested classes");
                    return setError();
                }
                else if (auto fdn = s.isFuncDeclaration())
                {
                    // make sure the parent context fdn of cd is reachable from sc
                    if (!ensureStaticLinkTo(sc.parent, fdn))
                    {
                        error(exp.loc, "outer function context of `%s` is needed to `new` nested class `%s`",
                            fdn.toPrettyChars(), cd.toPrettyChars());
                        return setError();
                    }
                }
                else
                    assert(0);
            }
            else if (exp.thisexp)
            {
                error(exp.loc, "`.new` is only for allocating nested classes");
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
                        error(exp.loc, "need `this` of type `%s` needed to `new` nested class `%s`", ad2.toChars(), cd.toChars());
                        return setError();
                    }
                }
            }

            if (cd.disableNew && !exp.onstack)
            {
                error(exp.loc, "cannot allocate `class %s` with `new` because it is annotated with `@disable new()`",
                          originalNewtype.toChars());
                return setError();
            }

            if (cd.ctor)
            {
                FuncDeclaration f = resolveFuncCall(exp.loc, sc, cd.ctor, null, tb, exp.argumentList, FuncResolveFlag.standard);
                if (!f || f.errors)
                    return setError();

                checkFunctionAttributes(exp, sc, f);
                if (!checkSymbolAccess(sc, f))
                {
                    error(exp.loc, "%s `%s` is not accessible from module `%s`",
                        f.kind(), f.toPrettyChars(), sc._module.toChars);
                    return setError();
                }

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
                    error(exp.loc, "no constructor for `%s`", cd.toChars());
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
                            v._init.isVoidInitializer() || v.semanticRun >= PASS.semantic2done)
                            continue;
                        v.inuse++;
                        v._init = v._init.initializerSemantic(v._scope, v.type, INITinterpret);
                        import dmd.semantic2 : lowerStaticAAs;
                        lowerStaticAAs(v, sc);
                        v.inuse--;
                    }
                }
            }

            // When using `@nogc` exception handling, lower `throw new E(args)` to
            // `throw (__tmp = _d_newThrowable!E(), __tmp.__ctor(args), __tmp)`.
            if (sc.previews.dip1008 && exp.thrownew &&
                !cd.isCOMclass() && !cd.isCPPclass())
            {
                assert(cd.ctor);

                Expression id = new IdentifierExp(exp.loc, Id.empty);
                id = new DotIdExp(exp.loc, id, Id.object);

                auto tiargs = new Objects();
                tiargs.push(exp.newtype);

                id = new DotTemplateInstanceExp(exp.loc, id, Id._d_newThrowable, tiargs);

                id = new CallExp(exp.loc, id).expressionSemantic(sc);

                exp.lowering = id.expressionSemantic(sc);

                result = exp;
                return;
            }
            else if (sc.needsCodegen() && // interpreter doesn't need this lowered
                     !exp.onstack && !exp.type.isScopeClass()) // these won't use the GC
            {
                /* replace `new T(arguments)` with `core.lifetime._d_newclassT!T(arguments)`
                 * or `_d_newclassTTrace`
                 */
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
                error(exp.loc, "default construction is disabled for type `%s`", sd.type.toChars());
                return setError();
            }
            // checkDeprecated() is already done in newtype.typeSemantic().

            if (sd.disableNew)
            {
                error(exp.loc, "cannot allocate `struct %s` with `new` because it is annotated with `@disable new()`",
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
                    if (arg && checkNewEscape(*sc, arg, false))
                        return setError();
                }
            }

            exp.type = exp.type.pointerTo();
            tryLowerToNewItem(exp);
        }
        else if (tb.ty == Tarray)
        {
            if (!nargs)
            {
                // https://issues.dlang.org/show_bug.cgi?id=20422
                // Without this check the compiler would give a misleading error
                error(exp.loc, "missing length argument for array");
                return setError();
            }

            Type tn = tb.nextOf().baseElemOf();
            Dsymbol s = tn.toDsymbol(sc);
            AggregateDeclaration ad = s ? s.isAggregateDeclaration() : null;
            if (ad && ad.noDefaultCtor)
            {
                error(exp.loc, "default construction is disabled for type `%s`", tb.nextOf().toChars());
                return setError();
            }
            for (size_t i = 0; i < nargs; i++)
            {
                if (tb.ty != Tarray)
                {
                    error(exp.loc, "too many arguments for array");
                    return setError();
                }

                Expression arg = (*exp.arguments)[i];
                if (exp.names && (*exp.names)[i])
                {
                    error(exp.loc, "no named argument `%s` allowed for array dimension", (*exp.names)[i].toChars());
                    return setError();
                }

                arg = resolveProperties(sc, arg);
                arg = arg.implicitCastTo(sc, Type.tsize_t);
                if (arg.op == EXP.error)
                    return setError();
                arg = arg.optimize(WANTvalue);
                if (arg.op == EXP.int64 && (target.isLP64 ?
                    cast(sinteger_t)arg.toInteger() : cast(int)arg.toInteger()) < 0)
                {
                    error(exp.loc, "negative array dimension `%s`", (*exp.arguments)[i].toChars());
                    return setError();
                }
                (*exp.arguments)[i] = arg;
                tb = tb.isTypeDArray().next.toBasetype();
            }

            if (!global.params.useGC && sc.needsCodegen())
            {
                version(IN_GCC)
                    error(exp.loc, "expression `%s` allocates with the GC and cannot be used with switch `-fno-rtti`", exp.toChars());
                else
                    error(exp.loc, "expression `%s` allocates with the GC and cannot be used with switch `-betterC`", exp.toChars());
                return setError();
            }

            if (!sc.needsCodegen())
                    goto LskipNewArrayLowering;

            /* Class types may inherit base classes that have errors.
                * This may leak errors from the base class to the derived one
                * and then to the hook. Semantic analysis is performed eagerly
                * to a void this.
                */
            if (auto tc = exp.type.nextOf.isTypeClass())
            {
                tc.sym.dsymbolSemantic(sc);
                if (tc.sym.errors)
                    goto LskipNewArrayLowering;
            }

            if (nargs == 1)
            {
                auto hook = global.params.tracegc ? Id._d_newarrayTTrace : Id._d_newarrayT;
                if (!verifyHookExist(exp.loc, *sc, hook, "new array"))
                    goto LskipNewArrayLowering;

                /* Lower the memory allocation and initialization of `new T[n]`
                 * to `_d_newarrayT!T(n)`.
                 */
                Expression lowering = new IdentifierExp(exp.loc, Id.empty);
                lowering = new DotIdExp(exp.loc, lowering, Id.object);
                auto tiargs = new Objects();
                /* Remove `inout`, `const`, `immutable` and `shared` to reduce
                 * the number of generated `_d_newarrayT` instances.
                 */
                const isShared = exp.type.nextOf.isShared();
                auto t = exp.type.nextOf.unqualify(MODFlags.wild | MODFlags.const_ |
                    MODFlags.immutable_ | MODFlags.shared_);
                tiargs.push(t);
                lowering = new DotTemplateInstanceExp(exp.loc, lowering, hook, tiargs);

                auto arguments = new Expressions();
                if (global.params.tracegc)
                {
                    auto funcname = (sc.callsc && sc.callsc.func) ?
                        sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
                    arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
                    arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
                    arguments.push(new StringExp(exp.loc, funcname.toDString()));
                }
                arguments.push((*exp.arguments)[0]);
                arguments.push(new IntegerExp(exp.loc, isShared, Type.tbool));

                lowering = new CallExp(exp.loc, lowering, arguments);
                exp.lowering = lowering.expressionSemantic(sc);
            }
            else
            {
                auto hook = global.params.tracegc ? Id._d_newarraymTXTrace : Id._d_newarraymTX;
                if (!verifyHookExist(exp.loc, *sc, hook, "new multi-dimensional array"))
                    goto LskipNewArrayLowering;

                /* Lower the memory allocation and initialization of `new T[][]...[](n1, n2, ...)`
                 * to `_d_newarraymTX!(T[][]...[], T)([n1, n2, ...])`.
                 */
                Expression lowering = new IdentifierExp(exp.loc, Id.empty);
                lowering = new DotIdExp(exp.loc, lowering, Id.object);

                auto tbn = exp.type.nextOf();
                size_t i = nargs;
                while (tbn.ty == Tarray && --i)
                    tbn = tbn.nextOf();
                auto unqualTbn = tbn.unqualify(MODFlags.wild | MODFlags.const_ |
                    MODFlags.immutable_ | MODFlags.shared_);

                auto tiargs = new Objects();
                tiargs.push(exp.type);
                tiargs.push(unqualTbn);
                lowering = new DotTemplateInstanceExp(exp.loc, lowering, hook, tiargs);

                auto arguments = new Expressions();
                if (global.params.tracegc)
                {
                    auto funcname = (sc.callsc && sc.callsc.func) ?
                        sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
                    arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
                    arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
                    arguments.push(new StringExp(exp.loc, funcname.toDString()));
                }

                arguments.push(new ArrayLiteralExp(exp.loc, Type.tsize_t.sarrayOf(nargs), exp.arguments));
                arguments.push(new IntegerExp(exp.loc, tbn.isShared(), Type.tbool));

                lowering = new CallExp(exp.loc, lowering, arguments);
                exp.lowering = lowering.expressionSemantic(sc);
            }
        }
        else if (tb.isScalar())
        {
            if (!nargs)
            {
            }
            else if (nargs == 1)
            {
                if (exp.names && (*exp.names)[0])
                {
                    error(exp.loc, "no named argument `%s` allowed for scalar", (*exp.names)[0].toChars());
                    return setError();
                }
                Expression e = (*exp.arguments)[0];
                e = e.implicitCastTo(sc, tb);
                (*exp.arguments)[0] = e;
            }
            else
            {
                error(exp.loc, "more than one argument for construction of `%s`", exp.type.toChars());
                return setError();
            }

            exp.type = exp.type.pointerTo();
            tryLowerToNewItem(exp);
        }
        else if (tb.ty == Taarray)
        {
            // e.g. `new Alias(args)`
            if (nargs)
            {
                error(exp.loc, "`new` cannot take arguments for an associative array");
                return setError();
            }
        }
        else
        {
            error(exp.loc, "cannot create a `%s` with `new`", exp.type.toChars());
            return setError();
        }

    LskipNewArrayLowering:
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
        sc.ctfe = false; // temporary stop CTFE
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
            if (f.checkNestedFuncReference(sc, e.loc))
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
            if (!functionSemantic(fd))
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
            // TODO: If fd isn't yet resolved its overload, the checkNestedFuncReference
            // call would cause incorrect validation.
            // Maybe here should be moved in CallExp, or AddrExp for functions.
            if (fd.checkNestedFuncReference(sc, e.loc))
                return setError();
        }
        else if (auto od = e.var.isOverDeclaration())
        {
            e.type = Type.tvoid; // ambiguous type?
        }

        result = e;
    }

    private void genIdent(FuncExp exp, Scope* sc)
    {
        if (exp.fd.ident != Id.empty)
            return;

        string s;
        if (exp.fd.fes)
            s = "__foreachbody";
        else if (exp.fd.tok == TOK.reserved)
            s = "__lambda";
        else if (exp.fd.tok == TOK.delegate_)
            s = "__dgliteral";
        else
            s = "__funcliteral";

        DsymbolTable symtab;
        if (FuncDeclaration func = sc.parent.isFuncDeclaration())
        {
            if (func.localsymtab is null)
            {
                // Inside template constraint, symtab is not set yet.
                // Initialize it lazily.
                func.localsymtab = new DsymbolTable();
            }
            symtab = func.localsymtab;
        }
        else
        {
            ScopeDsymbol sds = sc.parent.isScopeDsymbol();
            if (!sds.symtab)
            {
                // Inside template constraint, symtab may not be set yet.
                // Initialize it lazily.
                assert(sds.isTemplateInstance());
                sds.symtab = new DsymbolTable();
            }
            symtab = sds.symtab;
        }
        assert(symtab);
        Identifier id = Identifier.generateIdWithLoc(s, exp.loc, cast(string) toDString(sc.parent.toPrettyChars()));
        exp.fd.ident = id;
        if (exp.td)
            exp.td.ident = id;
        symtab.insert(exp.td ? cast(Dsymbol)exp.td : cast(Dsymbol)exp.fd);
    }

    override void visit(FuncExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("FuncExp::semantic(%s)\n", exp.toChars());
            if (exp.fd.treq)
                printf("  treq = %s\n", exp.fd.treq.toChars());
        }


        Expression e = exp;

        sc = sc.push(); // just create new scope
        sc.ctfe = false; // temporary stop CTFE
        sc.visibility = Visibility(Visibility.Kind.public_); // https://issues.dlang.org/show_bug.cgi?id=12506

        /* fd.treq might be incomplete type,
            * so should not semantic it.
            * void foo(T)(T delegate(int) dg){}
            * foo(a=>a); // in IFTI, treq == T delegate(int)
            */
        //if (fd.treq)
        //    fd.treq = fd.treq.dsymbolSemantic(loc, sc);

        genIdent(exp, sc);

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

        void done()
        {
            sc = sc.pop();
            result = e;
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
                if (exp.matchType(exp.fd.treq, sc, &fe, sc.eSink) > MATCH.nomatch)
                    e = fe;
                else
                    e = ErrorExp.get();
            }
            return done();
        }

        const olderrors = global.errors;
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
            return done();
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
                return done();
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
        done();
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
        if ((exp.type && exp.type != Type.tvoid) || !exp.td ||! arguments || !arguments.length)
            return exp.expressionSemantic(sc);

        for (size_t k = 0; k < arguments.length; k++)
        {
            Expression checkarg = (*arguments)[k];
            if (checkarg.op == EXP.error)
                return checkarg;
        }

        genIdent(exp, sc);

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
            error(exp.loc, "function literal `%s%s` is not callable using argument types `(%s)`",
                      exp.fd.toChars(), parametersTypeToChars(tfl.parameterList),
                      buf.peekChars());
            errorSupplemental(exp.loc, "too %s arguments, expected %d, got %d",
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

    override void visit(CallExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("CallExp::semantic() %s\n", exp.toChars());
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
                preFunctionParameters(sc, exp.argumentList, global.errorSink))
                return setError();

            // Run e1 semantic even if arguments have any errors
            exp.e1 = callExpSemantic(fe, sc, exp.arguments);
            if (exp.e1.op == EXP.error)
            {
                result = exp.e1;
                return;
            }
        }
        if (sc.inCfile)
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

        Type att = null;
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
                    error(exp.loc, "recursive evaluation of `%s`", exp.toChars());
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
                if (v && v.checkPurity(ve.loc, sc))
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
            else if (exp.e1.op == EXP.type && (sc && sc.inCfile))
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
                        error(arg.loc, "identifier or `(` expected");
                        result = ErrorExp.get();
                    }
                    return;
                }
                error(exp.loc, "identifier or `(` expected before `)`");
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
            preFunctionParameters(sc, exp.argumentList, global.errorSink))
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
                    if (ctor && (ctor.isCpCtor || ctor.isMoveCtor) && ctor.isGenerated())
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
                    e = new CallExp(exp.loc, e, exp.arguments, exp.names);
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
                    if (sd.aliasthis && !isRecursiveAliasThis(att, exp.e1.type))
                    {
                        exp.e1 = resolveAliasThis(sc, exp.e1);
                        goto Lagain;
                    }
                    error(exp.loc, "%s `%s` does not overload ()", sd.kind(), sd.toChars());
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
            else if (exp.e1.op == EXP.type && t1.isScalar())
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
                    error(exp.loc, "more than one argument for construction of `%s`", t1.toChars());
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
                auto f2 = resolveFuncCall(loc, sc, s, tiargs, tthis, argumentList, FuncResolveFlag.quiet);
                if (!f2)
                    continue;
                if (f2.errors)
                    return null;
                if (!f)
                {
                    f = f2;
                    continue;
                }
                /* Match in more than one overload set,
                 * even if one is a 'better' match than the other.
                 */
                if (f.isCsymbol() && f2.isCsymbol())
                {
                    /* C has global name space, so just pick one, such as f.
                     * If f and f2 are not compatible, that's how C rolls.
                     */
                }
                else
                    ScopeDsymbol.multiplyDefined(loc, f, f2); // issue error
            }
            if (f && f.errors)
                return null;
            if (f)
                return f;
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
                auto vi = findVtblIndex(exp.f, ad2.vtbl[]);
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
                    if (checkParamArgumentEscape(*sc, exp.f, Id.This, exp.f.vthis, STC.undefined_, ethis, false, false))
                        return setError();
                }
            }

            /* Cannot call public functions from inside invariant
             * (because then the invariant would have infinite recursion)
             */
            if (sc.func && sc.func.isInvariantDeclaration() && ue.e1.op == EXP.this_ && exp.f.addPostInvariant())
            {
                error(exp.loc, "cannot call `public`/`export` function `%s` from invariant", exp.f.toChars());
                return setError();
            }

            if (!exp.ignoreAttributes)
                checkFunctionAttributes(exp, sc, exp.f);

            // Cut-down version of checkAccess() that doesn't use the "most visible" version of exp.f.
            // We've already selected an overload here.
            const parent = exp.f.toParent();
            if (parent && parent.isTemplateInstance())
            {
                // already a deprecation
            }
            else if (!checkSymbolAccess(sc, exp.f))
            {
                error(exp.loc, "%s `%s` of type `%s` is not accessible from module `%s`",
                    exp.f.kind(), exp.f.toPrettyChars(), exp.f.type.toChars(), sc._module.toChars);
                return setError();
            }

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
                    error(exp.loc, "super class constructor call must be in a constructor");
                    return setError();
                }
                if (!cd.baseClass.ctor)
                {
                    error(exp.loc, "no super class constructor for `%s`", cd.baseClass.toChars());
                    return setError();
                }
            }
            else
            {
                // `this` call expression must be inside a
                // constructor
                if (!ad || !sc.func.isCtorDeclaration())
                {
                    error(exp.loc, "constructor call must be in a constructor");
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
                    error(exp.loc, "constructor calls not allowed in loops or after labels");
                if (sc.ctorflow.callSuper & (CSX.super_ctor | CSX.this_ctor))
                    error(exp.loc, "multiple constructor calls");
                if ((sc.ctorflow.callSuper & CSX.return_) && !(sc.ctorflow.callSuper & CSX.any_ctor))
                    error(exp.loc, "an earlier `return` statement skips constructor");
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
            if (!checkSymbolAccess(sc, exp.f))
            {
                error(exp.loc, "%s `%s` is not accessible from module `%s`",
                    exp.f.kind(), exp.f.toPrettyChars(), sc._module.toChars);
                return setError();
            }

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
                error(exp.loc, "cyclic constructor call");
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
            error(exp.loc, "function expected before `()`, not `%s`", exp.e1.toChars());
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
                exp.f = resolveFuncCall(exp.loc, sc, s, tiargs, null, exp.argumentList,
                    exp.isUfcsRewrite ? FuncResolveFlag.ufcs : FuncResolveFlag.standard);
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
                        return needThisError(exp.loc, exp.f);
                    }
                }
                exp.e1 = new VarExp(exp.e1.loc, exp.f, false);
                goto Lagain;
            }
            else
            {
                error(exp.loc, "function expected before `()`, not `%s` of type `%s`", exp.e1.toChars(), exp.e1.type.toChars());
                return setError();
            }

            void errorHelper(const(char)* failMessage) scope
            {
                OutBuffer buf;
                buf.writeByte('(');
                argExpTypesToCBuffer(buf, exp.arguments);
                buf.writeByte(')');
                if (tthis)
                    tthis.modToBuffer(buf);

                //printf("tf = %s, args = %s\n", tf.deco, (*arguments)[0].type.deco);
                .error(exp.loc, "%s `%s%s` is not callable using argument types `%s`",
                    p, exp.e1.toChars(), parametersTypeToChars(tf.parameterList), buf.peekChars());
                if (failMessage)
                    errorSupplemental(exp.loc, "%s", failMessage);
            }

            if (callMatch(exp.f, tf, null, exp.argumentList, 0, &errorHelper, sc) == MATCH.nomatch)
                return setError();

            // Purity and safety check should run after testing arguments matching
            if (exp.f)
            {
                exp.f.checkPurity(exp.loc, sc);
                exp.f.checkSafety(exp.loc, sc);
                exp.f.checkNogc(exp.loc, sc);
                if (exp.f.checkNestedFuncReference(sc, exp.loc))
                    return setError();
            }
            else if (sc.func && sc.intypeof != 1 && !(sc.ctfe || sc.debug_))
            {
                bool err = false;
                if (!tf.purity && sc.func.setImpure(exp.loc, "calling impure `%s`", exp.e1))
                {
                    error(exp.loc, "`pure` %s `%s` cannot call impure %s `%s`",
                        sc.func.kind(), sc.func.toPrettyChars(), p, exp.e1.toChars());
                    err = true;
                }
                if (!tf.isNogc && sc.func.setGC(exp.loc, "calling non-@nogc `%s`", exp.e1))
                {
                    error(exp.loc, "`@nogc` %s `%s` cannot call non-@nogc %s `%s`",
                        sc.func.kind(), sc.func.toPrettyChars(), p, exp.e1.toChars());
                    err = true;
                }
                if (tf.trust <= TRUST.system && sc.setUnsafe(true, exp.loc,
                    "calling `@system` `%s`", exp.e1))
                {
                    error(exp.loc, "`@safe` %s `%s` cannot call `@system` %s `%s`",
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

            if (ve.hasOverloads && exp.f.overnext)
                exp.f = resolveFuncCall(exp.loc, sc, exp.f, tiargs, null, exp.argumentList, FuncResolveFlag.overloadOnly);
            else
            {
                exp.f = exp.f.toAliasFunc();
                TypeFunction tf = cast(TypeFunction)exp.f.type;

                void errorHelper2(const(char)* failMessage) scope
                {
                    OutBuffer buf;
                    buf.writeByte('(');
                    argExpTypesToCBuffer(buf, exp.arguments);
                    buf.writeByte(')');

                    //printf("tf = %s, args = %s\n", tf.deco, (*arguments)[0].type.deco);
                    if (exp.isUfcsRewrite)
                    {
                        const arg = (*exp.argumentList.arguments)[0];
                        .error(exp.loc, "no property `%s` for `%s` of type `%s`", exp.f.ident.toChars(), arg.toChars(), arg.type.toChars());
                        .errorSupplemental(exp.loc, "the following error occured while looking for a UFCS match");
                    }

                    .error(exp.loc, "%s `%s` is not callable using argument types `%s`",
                        exp.f.kind(), exp.f.toChars(), buf.peekChars());
                    if (failMessage)
                        errorSupplemental(exp.loc, "%s", failMessage);
                    .errorSupplemental(exp.f.loc, "`%s%s` declared here", exp.f.toPrettyChars(), parametersTypeToChars(tf.parameterList));
                    exp.f = null;
                }

                if (callMatch(exp.f, tf, null, exp.argumentList, 0, &errorHelper2, sc) == MATCH.nomatch)
                    exp.f = null;
            }
            if (!exp.f || exp.f.errors)
                return setError();

            if (exp.f.needThis())
            {
                // Change the ancestor lambdas to delegate before hasThis(sc) call.
                if (exp.f.checkNestedFuncReference(sc, exp.loc))
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
                    return needThisError(exp.loc, exp.f);
                }
            }

            checkFunctionAttributes(exp, sc, exp.f);
            checkAccess(exp.loc, sc, null, exp.f);
            if (exp.f.checkNestedFuncReference(sc, exp.loc))
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
            error(exp.loc, "forward reference to inferred return type of function call `%s`", exp.toChars());
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

        // `super.fun()` with fun being abstract and unimplemented
        auto supDotFun = exp.e1.isDotVarExp();
        if (supDotFun && supDotFun.e1.isSuperExp() && exp.f && exp.f.isAbstract() && !exp.f.fbody)
        {
            error(exp.loc, "call to unimplemented abstract function `%s`", exp.f.toFullSignature());
            errorSupplemental(exp.loc, "declared here: %s", exp.f.loc.toChars());
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
                    error(exp.loc, "need `this` of type `%s` to call function `%s`", ad2.toChars(), exp.f.toChars());
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
        static if (LOGSEMANTIC)
        {
            printf("DeclarationExp::semantic() %s\n", e.toChars());
        }

        const olderrors = global.errors;

        /* This is here to support extern(linkage) declaration,
         * where the extern(linkage) winds up being an AttribDeclaration
         * wrapper.
         */
        Dsymbol s = e.declaration;

        while (1)
        {
            AttribDeclaration ad = s.isAttribDeclaration();
            if (!ad)
                break;
            if (ad.decl && ad.decl.length == 1)
                s = (*ad.decl)[0];
        }

        //printf("inserting '%s' %p into sc = %p\n", s.toChars(), s, sc);
        // Insert into both local scope and function scope.
        // Must be unique in both.
        if (s.ident)
        {
            VarDeclaration v = s.isVarDeclaration();
            if (v)
            {
                if (sc.inCfile)
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
                Dsymbol pscopesym;
                auto conflict = sc.search(Loc.initial, s.ident, pscopesym);
                error(e.loc, "declaration `%s` is already defined", s.toPrettyChars());
                errorSupplemental(conflict.loc, "`%s` `%s` is defined here",
                                  conflict.kind(), conflict.toChars());
                return setError();
            }

            if (v && sc.inCfile)
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
                        error(e.loc, "more than 65535 symbols with name `%s` generated", s.ident.toChars());
                        return setError();
                    }

                    // Replace originalSymbol with s, which updates the localCount
                    sc.func.localsymtab.update(s);

                    // The mangling change only works for D mangling
                }

                if (!sc.inCfile)
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
                        if (!scx.scopesym || !scx.scopesym.symtab)
                            continue;
                        Dsymbol s2 = scx.scopesym.symtab.lookup(s.ident);
                        if (s2 is null || s == s2)
                            continue;
                        // allow STC.local symbols to be shadowed
                        // TODO: not really an optimal design
                        auto decl = s2.isDeclaration();
                        if (decl && (decl.storage_class & STC.local))
                            continue;
                        if (sc.func.fes)
                        {
                            deprecation(e.loc, "%s `%s` is shadowing %s `%s`", s.kind(), s.ident.toChars(), s2.kind(), s2.toPrettyChars());
                            deprecationSupplemental(s2.loc, "declared here");
                        }
                        else
                        {
                            error(e.loc, "%s `%s` is shadowing %s `%s`", s.kind(), s.ident.toChars(), s2.kind(), s2.toPrettyChars());
                            errorSupplemental(s2.loc, "declared here");
                            return setError();
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
            error(exp.loc, "no type for `typeid(%s)`", ea ? ea.toChars() : (sa ? sa.toChars() : ""));
            return setError();
        }

        ta.checkComplexTransition(exp.loc, sc);

        auto tb = ta.toBasetype();
        if (ea && tb.ty == Tclass)
        {
            if (tb.toDsymbol(sc).isClassDeclaration().classKind == ClassKind.cpp)
            {
                error(exp.loc, "runtime type information is not supported for `extern(C++)` classes");
                return setError();
            }
            else if (!Type.typeinfoclass)
            {
                error(exp.loc, "`object.TypeInfo_Class` could not be found, but is implicitly used");
                return setError();
            }
            else
            {
                /* Get the dynamic type, which is .classinfo
                */
                ea = ea.expressionSemantic(sc);
                Expression e = new TypeidExp(ea.loc, ea);
                e.type = Type.typeinfoclass.type;
                result = e;
                return;
            }
        }
        else if (ta.ty == Terror)
        {
            return setError();
        }

        // Handle this in the glue layer
        Expression e = new TypeidExp(exp.loc, ta);

        e.type = getTypeInfoType(exp.loc, ta, sc);
        semanticTypeInfo(sc, ta);

        if (ea)
        {
            e = new CommaExp(exp.loc, ea, e); // execute ea
            e = e.expressionSemantic(sc);
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
                Dsymbol pscopesym;
                auto conflict = sc.search(Loc.initial, s.ident, pscopesym);
                error(e.loc, "declaration `%s` is already defined", s.toPrettyChars());
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
        if (e.id && !sc.condition)
        {
            error(e.loc, "can only declare type aliases within `static if` conditionals or `static assert`s");
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
            if (e.tok2 == TOK.module_ && !(p.isModule() || p.isPackageMod()))
                return no();
            tded = e.targ;
            return yes();
        }

        {
            Scope* sc2 = sc.copy(); // keep sc.flags
            sc2.tinst = null;
            sc2.minst = null;
            sc2.fullinst = true;
            Type t = dmd.typesem.trySemantic(e.targ, e.loc, sc2);
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
                        args.push(new Parameter(Loc.initial, STC.in_, b.type, null, null, null));
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
                if (e.targ.ty != Tfunction)
                    return no();
                goto case;
            case TOK.parameters:
                {
                    if (auto tf = e.targ.isFunction_Delegate_PtrToFunction())
                        tded = tf;
                    else
                        return no();

                    /* Generate tuple from function parameter types.
                     */
                    auto args = new Parameters();
                    foreach (i, arg; tded.isTypeFunction().parameterList)
                    {
                        assert(arg && arg.type);
                        /* If one of the default arguments was an error,
                           don't return an invalid tuple
                         */
                        if (e.tok2 == TOK.parameters && arg.defaultArg && arg.defaultArg.op == EXP.error)
                            return setError();
                        args.push(new Parameter(arg.loc, arg.storageClass, arg.type, (e.tok2 == TOK.parameters) ? arg.ident : null, (e.tok2 == TOK.parameters) ? arg.defaultArg : null, arg.userAttribDecl));
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

            if (e.tok != TOK.colon) /* == */
            {
                if (e.targ.equals(e.tspec))
                    return yes();
                else
                    return no();
            }

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

            MATCH m = deduceType(e.targ, sc, e.tspec, *e.parameters, dedtypes, null, 0, e.tok == TOK.equal);

            if (m == MATCH.nomatch || (m != MATCH.exact && e.tok == TOK.equal))
                return no();

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

                m = tp.matchArg(e.loc, sc, &tiargs, i, e.parameters, dedtypes, &s);
                if (m == MATCH.nomatch)
                    return no();
                s.dsymbolSemantic(sc);
                if (!sc.insert(s))
                {
                    Dsymbol pscopesym;
                    auto conflict = sc.search(Loc.initial, s.ident, pscopesym);
                    error(e.loc, "declaration `%s` is already defined", s.toPrettyChars());
                    errorSupplemental(conflict.loc, "`%s` `%s` is defined here",
                                        conflict.kind(), conflict.toChars());
                }

                unSpeculative(sc, s);
            }
            return yes();
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
        exp.e1 = exp.e1.modifiableLvalue(sc);
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

        if ((exp.op == EXP.addAssign || exp.op == EXP.minAssign) && exp.e1.type.toBasetype().ty == Tpointer && exp.e2.type.toBasetype().isIntegral())
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

    private Expression compileIt(MixinExp exp, Scope *sc)
    {
        OutBuffer buf;
        if (expressionsToString(buf, sc, exp.exps, exp.loc, null, true))
            return null;

        const errors = global.errors;
        const len = buf.length;
        const str = buf.extractChars()[0 .. len];
        const bool doUnittests = global.params.parsingUnittestsRequired();
        auto loc = adjustLocForMixin(str, exp.loc, global.params.mixinOut);
        scope p = new Parser!ASTCodegen(loc, sc._module, str, false, global.errorSink, &global.compileEnv, doUnittests);
        p.nextToken();
        //printf("p.loc.linnum = %d\n", p.loc.linnum);

        Expression e = p.parseExpression();
        if (global.errors != errors)
            return null;

        if (p.token.value != TOK.endOfFile)
        {
            error(e.loc, "unexpected token `%s` after %s expression",
                p.token.toChars(), EXPtoString(e.op).ptr);
            errorSupplemental(e.loc, "while parsing string mixin expression `%s`",
                str.ptr);
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

        // The expression is not treated as part of a default argument,
        // because it is evaluated at compile time.
        Scope* sc2 = sc.push();
        sc2.inDefaultArg = false;

        auto e = compileIt(exp, sc2);
        sc2.pop();
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
        if (!global.filePath.length)
        {
            error(e.loc, "need `-J` switch to import text file `%s`", namez.ptr);
            return setError();
        }

        /* Be wary of CWE-22: Improper Limitation of a Pathname to a Restricted Directory
         * ('Path Traversal') attacks.
         * https://cwe.mitre.org/data/definitions/22.html
         */

        if (FileName.absolute(namez))
        {
            error(e.loc, "absolute path is not allowed in import expression: `%s`", se.toChars());
            return setError();
        }

        auto idxReserved = FileName.findReservedChar(namez);
        if (idxReserved != size_t.max)
        {
            error(e.loc, "`%s` is not a valid filename on this platform", se.toChars());
            errorSupplemental(e.loc, "Character `'%c'` is reserved and cannot be used", namez[idxReserved]);
            return setError();
        }

        if (FileName.refersToParentDir(namez))
        {
            error(e.loc, "path refers to parent (`..`) directory: `%s`", se.toChars());
            return setError();
        }

        auto resolvedNamez = FileName.searchPath(global.filePath[], namez, false);
        if (!resolvedNamez)
        {
            error(e.loc, "file `%s` cannot be found or not in a path specified with `-J`", se.toChars());
            errorSupplemental(e.loc, "Path(s) searched (as provided by `-J`):");
            foreach (idx, path; global.filePath[])
            {
                const attr = FileName.exists(path);
                const(char)* err = attr == 2 ? "" :
                    (attr == 1 ? " (not a directory)" : " (path not found)");
                errorSupplemental(e.loc, "[%llu]: `%s`%s", cast(ulong)idx, path, err);
            }
            return setError();
        }

        sc._module.contentImportedFiles.push(resolvedNamez.ptr);
        if (global.params.v.verbose)
        {
            const slice = se.peekString();
            message("file      %.*s\t(%s)", cast(int)slice.length, slice.ptr, resolvedNamez.ptr);
        }

        addImportExpDep(global.params.moduleDeps, global.params.makeDeps, resolvedNamez, se.peekString(), sc._module);

        {
            auto fileName = FileName(resolvedNamez);
            if (auto fmResult = global.fileManager.getFileContents(fileName))
            {
                se = new StringExp(e.loc, fmResult);
                se.hexString = true;
            }
            else
            {
                error(e.loc, "cannot read file `%s`", resolvedNamez.ptr);
                return setError();
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
        if (auto e = exp.e1.isStringExp())
        {
            // deprecated in 2.107
            deprecation(e.loc, "assert condition cannot be a string literal");
            deprecationSupplemental(e.loc, "If intentional, use `%s !is null` instead to preserve behaviour",
                e.toChars());
        }

        const generateMsg = !exp.msg &&
                            sc.needsCodegen() && // let ctfe interpreter handle the error message
                            global.params.checkAction == CHECKACTION.context &&
                            global.params.useAssert == CHECKENABLE.on &&
                            !((exp.e1.isIntegerExp() && (exp.e1.toInteger() == 0)) ||
                               exp.e1.isNullExp());
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
                if (auto e1 = compileIt(ce, sc))
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
                buf.printf("`%s` failed", exp.toChars());
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
            checkParamArgumentEscape(*sc, null, null, null, STC.undefined_, exp.msg, true, false);
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

        te.type = Type.tnoreturn;
        if (throwSemantic(te.loc, te.e1, sc))
            result = te;
        else
            setError();
    }

    override void visit(DotIdExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotIdExp::semantic(this = %p, '%s')\n", exp, exp.toChars());
            printAST(exp);
        }

        if (sc.inCfile)
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
                    error(exp.e1.loc, "argument to `_Alignof` must be a type");
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
                    error(exp.e1.loc, "argument to `_Alignof` must be a type");
                    return setError();
                }
                else
                    assert(0);
                return;
            }

            if (exp.ident != Id.__sizeof)
            {
                result = fieldLookup(exp.e1, sc, exp.ident, exp.arrow);
                return;
            }
        }

        Expression e = exp.dotIdSemanticProp(sc, 1);

        if (e && isDotOpDispatch(e))
        {
            auto ode = e;
            const errors = global.startGagging();
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
                    error(exp.loc, "`%s` is not an expression", o.toChars());
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
            if (!functionSemantic(fd))
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
        // Indicate we need to resolve by UFCS.
        Expression e = exp.dotTemplateSemanticProp(sc, DotExpFlag.gag);
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
                error(e.loc, "%smethod `%s` is not callable using a %s`%s`",
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
                    error(e.loc, "need `this` of type `%s` to make delegate from function `%s`", ad2.toChars(), f.toChars());
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

        if (Expression ex = unaSemantic(exp, sc))
        {
            result = ex;
            return;
        }

        if (sc.inCfile)
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
                    exp.toLvalue(sc, "take address of");
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
                            if (sc.setUnsafe(false, exp.loc, "taking address of lazy parameter `%s`", ve))
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

        exp.e1 = exp.e1.toLvalue(sc, "take address of");
        if (exp.e1.op == EXP.error)
        {
            result = exp.e1;
            return;
        }
        if (checkNonAssignmentArrayOp(exp.e1))
            return setError();

        if (!exp.e1.type)
        {
            error(exp.loc, "cannot take address of `%s`", exp.e1.toChars());
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
            exp.e1.type = exp.e1.type.typeSemantic(exp.e1.loc, sc);
            if (!exp.e1.type.deco)  // still couldn't resolve it
            {
                if (auto ve = exp.e1.isVarExp())
                {
                    Declaration d = ve.var;
                    error(exp.loc, "forward reference to %s `%s`", d.kind(), d.toChars());
                }
                else
                    error(exp.loc, "forward reference to type `%s` of expression `%s`", exp.e1.type.toChars(), exp.e1.toChars());
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

                v.checkPurity(ve.loc, sc);
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
                    if (sc.func && !sc.intypeof && !sc.debug_)
                    {
                        sc.setUnsafe(false, exp.loc, "taking address of member `%s` without `this` reference", f);
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
            int deref;
            if (VarDeclaration v = expToVariable(exp.e1, deref))
            {
                v.checkPurity(exp.e1.loc, sc);
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
            error(exp.loc, "using `*` on an array is no longer supported; use `*(%s).ptr` instead", exp.e1.toChars());
            exp.type = (cast(TypeArray)tb).next;
            exp.e1 = exp.e1.castTo(sc, exp.type.pointerTo());
            break;

        case Terror:
            return setError();

        case Tnull:
            exp.type = Type.tnoreturn;  // typeof(*null) is bottom type
            break;

        default:
            error(exp.loc, "can only `*` a pointer, not a `%s`", exp.e1.type.toChars());
            goto case Terror;
        }

        if (sc.inCfile && exp.type && exp.type.toBasetype().ty == Tvoid)
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
        if (exp.e1.checkArithmetic(exp.op) ||
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
        if (exp.e1.checkArithmetic(exp.op))
            return setError();
        if (exp.e1.checkSharedAccess(sc))
            return setError();

        result = exp.e1;
    }

    override void visit(ComExp exp)
    {

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

        e.type = (sc && sc.inCfile) ? Type.tint32 : Type.tbool;
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
        exp.e1 = exp.e1.modifiableLvalue(sc);
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
            error(exp.loc, "cannot delete type `%s`", exp.e1.type.toChars());
            return setError();
        }

        ClassDeclaration cd = (cast(TypeClass)tb).sym;
        if (cd.isCOMinterface())
        {
            /* Because COM classes are deleted by IUnknown.Release()
             */
            error(exp.loc, "cannot `delete` instance of COM interface `%s`", cd.toChars());
            return setError();
        }

        bool err = false;
        if (cd.dtor)
        {
            err |= !functionSemantic(cd.dtor);
            err |= cd.dtor.checkPurity(exp.loc, sc);
            err |= cd.dtor.checkSafety(exp.loc, sc);
            err |= cd.dtor.checkNogc(exp.loc, sc);
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

        if ((sc && sc.inCfile) &&
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

        // https://issues.dlang.org/show_bug.cgi?id=24701
        auto r = checkNoreturnVarAccess(exp.e1);
        if (r != exp.e1 && exp.to && !exp.to.isTypeNoreturn())
        {
            result = r;
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
            error(exp.loc, "cannot cast `%s`", exp.e1.toChars());
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
            error(exp.loc, "cannot cast `%s` of type `%s` to type sequence `%s`", exp.e1.toChars(), exp.e1.type.toChars(), exp.to.toChars());
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

        Expression ex = exp.e1.castTo(sc, exp.to);
        if (ex.op == EXP.error)
        {
            result = ex;
            return;
        }

        // Check for unsafe casts
        string msg;
        if (!isSafeCast(ex, t1b, tob, msg))
        {
            if (sc.setUnsafe(false, exp.loc,
                "cast from `%s` to `%s`", exp.e1.type, exp.to))
            {
                if (msg.length)
                    errorSupplemental(exp.loc, "%.*s", msg.fTuple.expand);
                return setError();
            }
        }
        else if (msg.length) // deprecated unsafe
        {
            const err = sc.setUnsafePreview(FeatureState.default_, false, exp.loc,
                "cast from `%s` to `%s`", exp.e1.type, exp.to);
            // if message was printed
            if (sc.func && sc.func.isSafeBypassingInference() && !sc.isDeprecated())
                deprecationSupplemental(exp.loc, "%.*s", msg.fTuple.expand);
            if (err)
                return setError();
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

            if(t1b.ty == Tarray && exp.e1.op != EXP.arrayLiteral && sc.needsCodegen())
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

        if (sc && sc.inCfile)
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

             error(exp.loc, "constant expression expected, not `%s`", elem.toChars());
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
                error(exp.loc, "cannot slice type `%s`", exp.e1.toChars());
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
                error(exp.loc, "cannot slice function pointer `%s`", exp.e1.toChars());
                return setError();
            }
            if (!exp.lwr || !exp.upr)
            {
                error(exp.loc, "upper and lower bounds are needed to slice a pointer");
                if (auto ad = isAggregate(tp.next.toBasetype()))
                {
                    auto s = search_function(ad, Id.index);
                    if (!s) s = search_function(ad, Id.slice);
                    if (s)
                    {
                        auto fd = s.isFuncDeclaration();
                        if ((fd && !fd.getParameterList().length) || s.isTemplateDeclaration())
                        {
                            errorSupplemental(exp.loc,
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
            if (sc.setUnsafe(false, exp.loc, "pointer slicing"))
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
                error(exp.loc, "need upper and lower bound to slice a sequence");
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
            error(exp.loc, "`%s` cannot be sliced with `[]`", t1b.ty == Tvoid ? exp.e1.toChars() : t1b.toChars());
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
                error(exp.loc, "string slice `[%llu .. %llu]` is out of bounds", i1, i2);
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

        if (sc.inCfile)
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

        if (auto ad = isAggregate(exp.e1.type))
        {
            error(exp.loc, "no `[]` operator overload for type `%s`", exp.e1.type.toChars());
            errorSupplemental(ad.loc, "`%s` declared here", ad.toPrettyChars());
        }
        else if (exp.e1.op == EXP.type && exp.e1.type.ty != Ttuple)
            error(exp.loc, "static array of `%s` with multiple lengths not allowed", exp.e1.type.toChars());
        else if (isIndexableNonAggregate(exp.e1.type))
            error(exp.loc, "only one index allowed to index `%s`", exp.e1.type.toChars());
        else
            error(exp.loc, "cannot use `[]` operator on expression of type `%s`", exp.e1.type.toChars());

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

        if (sc.inCfile)
            return;

        if (!e.isGenerated)
        {
            if (e.allowCommaExp)
            {
                checkMustUse(e.e1, sc);
                discardValue(e.e1);
            }
            else
                error(e.loc, "using the result of a comma expression is not allowed");
        }
    }

    override void visit(IntervalExp e)
    {
        static if (LOGSEMANTIC)
        {
            printf("IntervalExp::semantic('%s')\n", e.toChars());
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
                error(exp.loc, "cannot index function pointer `%s`", exp.e1.toChars());
                return setError();
            }
            exp.e2 = exp.e2.implicitCastTo(sc, Type.tsize_t);
            if (exp.e2.type == Type.terror)
                return setError();
            exp.e2 = exp.e2.optimize(WANTvalue);
            if (exp.e2.op == EXP.int64 && exp.e2.toInteger() == 0)
            {
            }
            else if (sc.setUnsafe(false, exp.loc, "indexing pointer `%s`", exp.e1))
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
                checkNewEscape(*sc, exp.e2, false);

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
                    error(exp.loc, "sequence index `[%llu]` is outside bounds `[0 .. %llu]`", index, cast(ulong)length);
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
            error(exp.loc, "`%s` must be an array or pointer type, not `%s`", exp.e1.toChars(), exp.e1.type.toChars());
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
                else if (sc.inCfile && t1b.ty == Tsarray)
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

        if (sc.inCfile)
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
            error(exp.loc, "cannot post-%s array slice `%s`, use pre-%s instead", s, exp.e1.toChars(), s);
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

        exp.e1 = exp.e1.modifiableLvalue(sc);
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
            if (exp.op == EXP.blit)      printf("BlitExp.semantic('%s')\n", exp.toChars());
            if (exp.op == EXP.assign)    printf("AssignExp.semantic('%s')\n", exp.toChars());
            if (exp.op == EXP.construct) printf("ConstructExp.semantic('%s')\n", exp.toChars());
        }

        void setResult(Expression e, int line = __LINE__)
        {
            //printf("line %d\n", line);
            result = e;
        }

        Expression e1old = exp.e1;

        if (auto e2comma = exp.e2.isCommaExp())
        {
            if (!e2comma.isGenerated && !sc.inCfile)
                error(exp.loc, "using the result of a comma expression is not allowed");

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
            Type att = null; // first cyclic `alias this` type
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
                if (ad.aliasthis && !isRecursiveAliasThis(att, ae.e1.type))
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
                Expression e = dti.dotTemplateSemanticProp(sc, DotExpFlag.gag);
                if (!e)
                {
                    return setResult(resolveUFCSProperties(sc, e1x, exp.e2));
                }

                e1x = e;
            }
            else if (sc.inCfile && e1x.isDotIdExp())
            {
                auto die = e1x.isDotIdExp();
                e1x = fieldLookup(die.e1, sc, die.ident, die.arrow);
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
                    const errors = global.startGagging();
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
            if (Expression e = resolvePropertiesX(sc, e1x, exp.e2, exp))
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
                    error(exp.loc, "mismatched sequence lengths, %d and %d", cast(int)dim, cast(int)tup2.exps.length);
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
                        if (checkConstructorEscape(*sc, cx, false))
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

                        //printf("exp: %s\n", toChars(exp));
                        //printf("e2x: %s\n", toChars(e2x));
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
                                    error(exp.loc, "conversion error from `%s` to `%s`",
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
                        else if (sd.hasMoveCtor && !e2x.isCallExp() && !e2x.isStructLiteralExp())
                        {
                            // #move
                            /* The !e2x.isCallExp() is because it is already an rvalue
                               and the move constructor is unnecessary:
                                struct S {
                                    alias TT this;
                                    long TT();
                                    this(T)(int x) {}
                                    this(S);
                                    this(ref S);
                                    ~this();
                                }
                                S fun(ref S arg);
                                void test() { S st; fun(st); }
                             */
                            /* Rewrite as:
                             * e1 = init, e1.moveCtor(e2);
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
                            ex = ex.modifiableLvalue(sc); // allocate new slot
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
            if (sc.inCfile && e2x.isStringExp() && t2.isTypeSArray())
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
                    if (t1.checkPostblit(e1x.loc, sc))
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
                        error(exp.loc, "mismatched array lengths, %d and %d", cast(int)dim1, cast(int)dim2);
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
                            error(exp.loc, "static array `%s` size overflowed to %llu",
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

            auto ale1x = ale.e1.modifiableLvalueImpl(sc, exp.e1);
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

            if (!sc.needsCodegen())      // if compile time creature only
            {
                exp.type = Type.tsize_t;
                return setResult(exp);
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

            Expression ce = new CallExp(ale.loc, id, arguments).expressionSemantic(sc);
            auto res = new LoweredAssignExp(exp, ce);
            // if (global.params.verbose)
            //     message("lowered   %s =>\n          %s", exp.toChars(), res.toChars());
            res.type = Type.tsize_t;
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
                error(exp.loc, "slice `%s` is not mutable", se.toChars());
                return setError();
            }

            if (exp.op == EXP.assign && !tn.baseElemOf().isAssignable())
            {
                error(exp.loc, "slice `%s` is not mutable, struct `%s` has immutable members",
                    exp.e1.toChars(), tn.baseElemOf().toChars());
                result = ErrorExp.get();
                return;
            }

            // For conditional operator, both branches need conversion.
            while (se.e1.op == EXP.slice)
                se = cast(SliceExp)se.e1;
            if (se.e1.op == EXP.question && se.e1.type.toBasetype().ty == Tsarray)
            {
                se.e1 = se.e1.modifiableLvalueImpl(sc, exp.e1);
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
                    error(exp.loc, "array `%s` is not mutable, struct `%s` has immutable members",
                        exp.e1.toChars(), tn.baseElemOf().toChars());
                    result = ErrorExp.get();
                    return;
                }
            }

            Expression e1x = exp.e1;

            // Try to do a decent error message with the expression
            // before it gets constant folded
            if (exp.op == EXP.assign)
                e1x = e1x.modifiableLvalueImpl(sc, e1old);

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
            if (exp.op != EXP.blit && e2x.isLvalue() && t1.nextOf.checkPostblit(exp.e1.loc, sc))
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
                    error(exp.loc, "mismatched array lengths %d and %d for assignment `%s`", cast(int)dim1, cast(int)dim2, exp.toChars());
                    return setError();
                }
            }

            if (exp.op != EXP.blit &&
                (e2x.op == EXP.slice && (cast(UnaExp)e2x).e1.isLvalue() ||
                 e2x.op == EXP.cast_ && (cast(UnaExp)e2x).e1.isLvalue() ||
                 e2x.op != EXP.slice && e2x.isLvalue()))
            {
                if (t1.nextOf().checkPostblit(exp.e1.loc, sc))
                    return setError();
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
        }
        else
        {
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

        if (exp.e1.op == EXP.slice &&
            (t1.ty == Tarray || t1.ty == Tsarray) &&
            t1.nextOf().toBasetype().ty == Tvoid)
        {
            if (t2.nextOf().implicitConvTo(t1.nextOf()))
            {
                if (sc.setUnsafe(false, exp.loc, "copying `%s` to `%s`", t2, t1))
                    return setError();
            }
            else
            {
                // copying from non-void to void was overlooked, deprecate
                if (sc.setUnsafePreview(FeatureState.default_, false, exp.loc,
                    "copying `%s` to `%s`", t2, t1))
                    return setError();
            }
            if (sc.previews.fixImmutableConv && !t2.implicitConvTo(t1))
            {
                error(exp.loc, "cannot copy `%s` to `%s`",
                    t2.toChars(), t1.toChars());
                errorSupplemental(exp.loc,
                    "Source data has incompatible type qualifier(s)");
                errorSupplemental(exp.loc, "Use `cast(%s)` to force copy", t1.toChars());
                return setError();
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
                error(exp.loc, "cannot rebind scope variables");
            }
        }

        if (exp.e1.op == EXP.variable && (cast(VarExp)exp.e1).var.ident == Id.ctfe)
        {
            error(exp.loc, "cannot modify compiler-generated variable `__ctfe`");
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
            checkAssignEscape(*sc, res, false, false);
        else
            checkNewEscape(*sc, assignElem, false); // assigning to AA puts it on heap

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

            if (!sc.needsCodegen()) // interpreter can handle these
                return setResult(res);

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

                if (global.params.v.verbose)
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

        if (global.params.v.verbose)
            message("lowered   %s =>\n          %s", ae.toChars(), res.toChars());

        res = new LoweredAssignExp(ae, res);
        res.type = ae.type;

        return res;
    }

    override void visit(PowAssignExp exp)
    {

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
            if ((tb1.isIntegral() || tb1.isFloating()) && (tb2.isIntegral() || tb2.isFloating()))
            {
                exp.type = exp.e1.type;
                result = arrayOp(exp, sc);
                return;
            }
        }
        else
        {
            exp.e1 = exp.e1.modifiableLvalue(sc);
        }

        if ((exp.e1.type.isIntegral() || exp.e1.type.isFloating()) && (exp.e2.type.isIntegral() || exp.e2.type.isFloating()))
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
                error(exp.loc, "cannot append to static array `%s`", se.e1.type.toChars());
                return setError();
            }
        }

        exp.e1 = exp.e1.modifiableLvalue(sc);
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
             // Do not strip const(void)[]
             (!sc.previews.fixImmutableConv || tb1next.ty != Tvoid) &&
              (tb2.nextOf().size(Loc.initial) == tb1next.size(Loc.initial)))))
        {
            // EXP.concatenateAssign
            assert(exp.op == EXP.concatenateAssign);
            if (tb1next.checkPostblit(exp.e1.loc, sc))
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
            if (tb2.checkPostblit(exp.e2.loc, sc))
                return setError();

            if (checkNewEscape(*sc, exp.e2, false))
                return setError();

            auto ecast = exp.e2.castTo(sc, tb1next);
            if (auto ce = ecast.isCastExp())
                ce.trusted = true;

            exp = new CatElemAssignExp(exp.loc, exp.type, exp.e1, ecast);
            exp.e2 = doCopyOrMove(sc, exp.e2, null, false);
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

            error(exp.loc, "cannot append type `%s` to type `%s`", tb2.toChars(), tb1.toChars());
            return setError();
        }

        if (exp.e2.checkValue() || exp.e2.checkSharedAccess(sc))
            return setError();

        exp.type = exp.e1.type;
        auto assignElem = exp.e2;
        auto res = exp.reorderSettingAAElem(sc);
        if (res != exp) // `AA[k] = v` rewrite was performed
            checkNewEscape(*sc, assignElem, false);
        else if (exp.op == EXP.concatenateElemAssign || exp.op == EXP.concatenateDcharAssign)
            checkAssignEscape(*sc, res, false, false);

        result = res;

        if ((exp.op == EXP.concatenateAssign || exp.op == EXP.concatenateElemAssign) && sc.needsCodegen())
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

                exp.lowering = ce.expressionSemantic(sc);
                *output = exp;
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
                if (!verifyHookExist(exp.loc, *sc, hook, "appending element to arrays", Id.object))
                    return setError();

                // Lower to object._d_arrayappendcTX{,Trace}(e1, 1), e1[$-1]=e2
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
                     * by calling `_d_arrayappendcTX`, so we need to save `a[$-1]` in
                     * a temporary variable.
                     */
                    value2 = extractSideEffect(sc, "__appendtmp", eValue2, value2, true);

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

                exp.lowering = e0.expressionSemantic(sc);
                *output = exp;
            }
        }
    }

    override void visit(AddExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("AddExp::semantic('%s')\n", exp.toChars());
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
            err |= exp.e1.checkArithmetic(exp.op) || exp.e1.checkSharedAccess(sc);
        }
        if (tb2.ty == Tdelegate || tb2.isPtrToFunction())
        {
            err |= exp.e2.checkArithmetic(exp.op) || exp.e2.checkSharedAccess(sc);
        }
        if (err)
            return setError();

        if (tb1.ty == Tpointer && exp.e2.type.isIntegral() || tb2.ty == Tpointer && exp.e1.type.isIntegral())
        {
            result = scaleFactor(exp, sc);
            return;
        }

        if (tb1.ty == Tpointer && tb2.ty == Tpointer ||
            tb1.ty == Tnull && tb2.ty == Tnull)
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
        if ((tb1.isReal() && exp.e2.type.isImaginary()) || (tb1.isImaginary() && exp.e2.type.isReal()))
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
            err |= exp.e1.checkArithmetic(exp.op) || exp.e1.checkSharedAccess(sc);
        }
        if (t2.ty == Tdelegate || t2.isPtrToFunction())
        {
            err |= exp.e2.checkArithmetic(exp.op) || exp.e2.checkSharedAccess(sc);
        }
        if (t1.ty == Tnull && t2.ty == Tnull)
        {
            exp.incompatibleTypes();
            return setError();
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
                    // See https://github.com/dlang/dmd/pull/7332
                    error(exp.loc, "cannot subtract pointers to different types: `%s` and `%s`.",
                        t1.toChars(), t2.toChars());
                    return setError();
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
            else if (t2.isIntegral())
                e = scaleFactor(exp, sc);
            else
            {
                error(exp.loc, "can't subtract `%s` from pointer", t2.toChars());
                e = ErrorExp.get();
            }
            result = e;
            return;
        }
        if (t2.ty == Tpointer)
        {
            exp.type = exp.e2.type;
            error(exp.loc, "can't subtract pointer from `%s`", exp.e1.type.toChars());
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
        if ((t1.isReal() && t2.isImaginary()) || (t1.isImaginary() && t2.isReal()))
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

    /**
     * If the given expression is a `CatExp`, the function tries to lower it to
     * `_d_arraycatnTX`.
     *
     * Params:
     *      ee = the `CatExp` to lower
     * Returns:
     *      `_d_arraycatnTX(e1, e2, ..., en)` if `ee` is `e1 ~ e2 ~ ... en`
     *      `ee` otherwise
     */
    private Expression lowerToArrayCat(CatExp exp)
    {
        // String literals are concatenated by the compiler. No lowering is needed.
        if ((exp.e1.isStringExp() && (exp.e2.isIntegerExp() || exp.e2.isStringExp())) ||
            (exp.e2.isStringExp() && (exp.e1.isIntegerExp() || exp.e1.isStringExp())))
            return exp;

        bool useTraceGCHook = global.params.tracegc && sc.needsCodegen();

        Identifier hook = useTraceGCHook ? Id._d_arraycatnTXTrace : Id._d_arraycatnTX;
        if (!verifyHookExist(exp.loc, *sc, hook, "concatenating arrays"))
        {
            setError();
            return result;
        }

        void handleCatArgument(Expressions *arguments, Expression e, Type catType, bool isRightArg)
        {
            auto tb = e.type.toBasetype();

            if ((isRightArg && e.parens) || (!isRightArg && !tb.equals(catType)))
            {
                arguments.push(e);
                return;
            }

            if (auto ce = e.isCatExp())
            {
                Expression lowering = ce.lowering;

                /* Skip `file`, `line`, and `funcname` if the hook of the parent
                 * `CatExp` is `_d_arraycatnTXTrace`.
                 */
                if (auto callExp = isRuntimeHook(lowering, hook))
                {
                    if (hook == Id._d_arraycatnTX)
                        arguments.pushSlice((*callExp.arguments)[]);
                    else
                        arguments.pushSlice((*callExp.arguments)[3 .. $]);
                }
            }
            else
                arguments.push(e);
        }

        auto arguments = new Expressions();
        if (useTraceGCHook)
        {
            auto funcname = (sc.callsc && sc.callsc.func) ?
                sc.callsc.func.toPrettyChars() : sc.func.toPrettyChars();
            arguments.push(new StringExp(exp.loc, exp.loc.filename.toDString()));
            arguments.push(new IntegerExp(exp.loc, exp.loc.linnum, Type.tint32));
            arguments.push(new StringExp(exp.loc, funcname.toDString()));
        }

        handleCatArgument(arguments, exp.e1, exp.type.toBasetype(), false);
        handleCatArgument(arguments, exp.e2, exp.type.toBasetype(), true);

        Expression id = new IdentifierExp(exp.loc, Id.empty);
        id = new DotIdExp(exp.loc, id, Id.object);

        auto tiargs = new Objects();
        tiargs.push(exp.type);
        id = new DotTemplateInstanceExp(exp.loc, id, hook, tiargs);
        id = new CallExp(exp.loc, id, arguments);
        return id.expressionSemantic(sc);
    }

    void trySetCatExpLowering(Expression exp)
    {
        /* `_d_arraycatnTX` canot be used with `-betterC`, but `CatExp`s may be
         * used with `-betterC`, but only during CTFE.
         */
        if (!global.params.useGC)
            return;

        if (auto ce = exp.isCatExp())
            ce.lowering = lowerToArrayCat(ce);
    }

    override void visit(CatExp exp)
    {
        // https://dlang.org/spec/expression.html#cat_expressions
        //printf("CatExp.semantic() %s\n", toChars());

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
                exp.e2 = doCopyOrMove(sc, exp.e2, null, false);
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
                if (tb2.checkPostblit(exp.e2.loc, sc))
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
                if (checkNewEscape(*sc, exp.e2, false))
                    return setError();
                result = exp.optimize(WANTvalue);
                trySetCatExpLowering(result);
                return;
            }
        }
        // Check for: element ~ array
        if ((tb2.ty == Tsarray || tb2.ty == Tarray) && tb1.ty != Tvoid)
        {
            if (exp.e2.op == EXP.arrayLiteral)
            {
                exp.e1 = doCopyOrMove(sc, exp.e1, null, false);
            }
            else if (exp.e2.op == EXP.string_)
            {
            }
            else
            {
                if (tb1.checkPostblit(exp.e1.loc, sc))
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
                if (checkNewEscape(*sc, exp.e1, false))
                    return setError();
                result = exp.optimize(WANTvalue);
                trySetCatExpLowering(result);
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
            trySetCatExpLowering(result);
            return;
        }
        exp.type = exp.type.toHeadMutable();

        Type tb = exp.type.toBasetype();
        if (tb.ty == Tsarray)
            exp.type = tb.nextOf().arrayOf();
        if (exp.type.ty == Tarray && tb1next && tb2next && tb1next.mod != tb2next.mod)
        {
            // Do not strip const(void)[]
            if (!sc.previews.fixImmutableConv || tb.nextOf().ty != Tvoid)
                exp.type = exp.type.nextOf().toHeadMutable().arrayOf();
        }
        if (Type tbn = tb.nextOf())
        {
            if (tbn.checkPostblit(exp.loc, sc))
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
        trySetCatExpLowering(result);
    }

    override void visit(MulExp exp)
    {
        version (none)
        {
            printf("MulExp::semantic() %s\n", exp.toChars());
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

        if (exp.type.isFloating())
        {
            Type t1 = exp.e1.type;
            Type t2 = exp.e2.type;

            if (t1.isReal())
            {
                exp.type = t2;
            }
            else if (t2.isReal())
            {
                exp.type = t1;
            }
            else if (t1.isImaginary())
            {
                if (t2.isImaginary())
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
            else if (t2.isImaginary())
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

        if (exp.type.isFloating())
        {
            Type t1 = exp.e1.type;
            Type t2 = exp.e2.type;

            if (t1.isReal())
            {
                exp.type = t2;
                if (t2.isImaginary())
                {
                    // x/iv = i(-x/v)
                    exp.e2.type = t1;
                    e = new NegExp(exp.loc, exp);
                    e = e.expressionSemantic(sc);
                    result = e;
                    return;
                }
            }
            else if (t2.isReal())
            {
                exp.type = t1;
            }
            else if (t1.isImaginary())
            {
                if (t2.isImaginary())
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
            else if (t2.isImaginary())
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

        if (exp.type.isFloating())
        {
            exp.type = exp.e1.type;
            if (exp.e2.type.isComplex())
            {
                error(exp.loc, "cannot perform modulo complex arithmetic");
                return setError();
            }
        }
        result = exp;
    }

    override void visit(PowExp exp)
    {

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
            error(e.loc, "`%s` requires `std.math` for `^^` operators", e.toChars());
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

    private void visitShift(BinExp exp)
    {

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
        {
            Type tb1 = exp.e1.type.toBasetype();
            exp.e2 = exp.e2.castTo(sc, tb1.ty == Tvector ? tb1 : Type.tshiftcnt);
        }

        exp.type = exp.e1.type;
        result = exp;
    }

    override void visit(ShlExp exp)
    {
        visitShift(exp);
    }
    override void visit(ShrExp exp)
    {
        visitShift(exp);
    }
    override void visit(UshrExp exp)
    {
        visitShift(exp);
    }

    private void visitBinaryBitOp(BinExp exp)
    {

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

    override void visit(AndExp exp)
    {
        visitBinaryBitOp(exp);
    }
    override void visit(OrExp exp)
    {
        visitBinaryBitOp(exp);
    }
    override void visit(XorExp exp)
    {
        visitBinaryBitOp(exp);
    }

    override void visit(LogicalExp exp)
    {
        static if (LOGSEMANTIC)
        {
            printf("LogicalExp::semantic() %s\n", exp.toChars());
        }


        exp.setNoderefOperands();

        Expression e1x = exp.e1.expressionSemantic(sc);

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (e1x.op == EXP.type)
            e1x = resolveAliasThis(sc, e1x);

        e1x = resolveProperties(sc, e1x);
        e1x = e1x.toBoolean(sc);

        if (sc.condition)
        {
            /* If in static if, don't evaluate e2 if we don't have to.
             */
            e1x = e1x.optimize(WANTvalue);
            if (e1x.toBool().hasValue(exp.op == EXP.orOr))
            {
                if (sc.inCfile)
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
            error(exp.loc, "`%s` is not an expression", exp.e2.toChars());
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
            exp.type = (sc && sc.inCfile) ? Type.tint32 : Type.tbool;

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
            error(exp.loc, "do not use `null` when comparing class types");
            return setError();
        }


        EXP cmpop = exp.op;
        if (auto e = exp.op_overload(sc, &cmpop))
        {
            if (!e.type.isScalar() && e.type.equals(exp.e1.type))
            {
                error(exp.loc, "recursive `opCmp` expansion");
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

        exp.type = (sc && sc.inCfile) ? Type.tint32 : Type.tbool;

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
                error(exp.loc, "array comparison type mismatch, `%s` vs `%s`", t1next.toChars(), t2next.toChars());
                return setError();
            }

            if ((t1.ty == Tarray || t1.ty == Tsarray) &&
                (t2.ty == Tarray || t2.ty == Tsarray))
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
                error(exp.loc, "need member function `opCmp()` for %s `%s` to compare", t2.toDsymbol(sc).kind(), t2.toChars());
            else
                error(exp.loc, "need member function `opCmp()` for %s `%s` to compare", t1.toDsymbol(sc).kind(), t1.toChars());
            return setError();
        }
        else if (t1.isComplex() || t2.isComplex())
        {
            error(exp.loc, "compare not defined for complex operands");
            return setError();
        }
        else if (t1.ty == Taarray || t2.ty == Taarray)
        {
            error(exp.loc, "`%s` is not defined for associative arrays", EXPtoString(exp.op).ptr);
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
            errorSupplemental(exp.loc, "`in` is only allowed on associative arrays");
            const(char)* slice = (t2b.ty == Tsarray) ? "[]" : "";
            errorSupplemental(exp.loc, "perhaps use `std.algorithm.find(%s, %s%s)` instead",
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
                error(exp.loc, "comparison between different enumeration types `%s` and `%s`; If this behavior is intended consider using `std.conv.asOriginalType`",
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

        exp.type = (sc && sc.inCfile) ? Type.tint32 : Type.tbool;

        if (!isArrayComparison)
        {
            if (exp.e1.type != exp.e2.type && exp.e1.type.isFloating() && exp.e2.type.isFloating())
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
                error(exp.loc, "incompatible types for array comparison: `%s` and `%s`",
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

        if (exp.e1.type != exp.e2.type && exp.e1.type.isFloating() && exp.e2.type.isFloating())
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
            deprecation(exp.loc, "identity comparison of static arrays "
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
        if (sc.inCfile)
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
        result = e.resolveLoc(e.loc, sc);
    }

    override void visit(LineInitExp e)
    {
        e.type = Type.tint32;
        result = e.resolveLoc(e.loc, sc);
    }

    override void visit(ModuleInitExp e)
    {
        //printf("ModuleInitExp::semantic()\n");
        e.type = Type.tstring;
        result = e.resolveLoc(e.loc, sc);
    }

    override void visit(FuncInitExp e)
    {
        //printf("FuncInitExp::semantic()\n");
        e.type = Type.tstring;
        result = e.resolveLoc(e.loc, sc);
    }

    override void visit(PrettyFuncInitExp e)
    {
        //printf("PrettyFuncInitExp::semantic()\n");
        e.type = Type.tstring;
        result = e.resolveLoc(e.loc, sc);
    }
}

/**********************************
 * Try to run semantic routines.
 * If they fail, return NULL.
 */
Expression trySemantic(Expression exp, Scope* sc)
{
    //printf("+trySemantic(%s)\n", exp.toChars());
    const errors = global.startGagging();
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

/// Returns: whether expressionSemantic() has been run on expression `e`
private bool expressionSemanticDone(Expression e)
{
    // Usually, Expression.type gets set by expressionSemantic and is `null` beforehand
    // There are some exceptions however:
    return e.type !is null && !(
        e.isRealExp() // type sometimes gets set already before semantic
        || e.isTypeExp() // stores its type in the Expression.type field
        || e.isCompoundLiteralExp() // stores its `(type) {}` in type field, gets rewritten to struct literal
        || e.isVarExp() // type sometimes gets set already before semantic
    );
}

// entrypoint for semantic ExpressionSemanticVisitor
Expression expressionSemantic(Expression e, Scope* sc)
{
    if (e.expressionSemanticDone)
        return e;

    scope v = new ExpressionSemanticVisitor(sc);
    e.accept(v);
    return v.result;
}

private Expression dotIdSemanticPropX(DotIdExp exp, Scope* sc)
{
    //printf("dotIdSemanticPropX() %s\n", toChars(exp));
    if (Expression ex = unaSemantic(exp, sc))
        return ex;

    if (!sc.inCfile && exp.ident == Id._mangleof)
    {
        // symbol.mangleof

        // return mangleof as an Expression
        static Expression dotMangleof(const ref Loc loc, Scope* sc, Dsymbol ds, bool hasOverloads)
        {
            Expression e;

            assert(ds);
            if (auto f = ds.isFuncDeclaration())
            {
                if (f.checkForwardRef(loc))
                    return ErrorExp.get();

                if (f.purityInprocess || f.safetyInprocess || f.nothrowInprocess || f.nogcInprocess)
                {
                    error(loc, "%s `%s` cannot retrieve its `.mangleof` while inferring attributes", f.kind, f.toPrettyChars);
                    return ErrorExp.get();
                }

                if (!hasOverloads)
                    e = StringExp.create(loc, mangleExact(f));
            }

            if (!e)
            {
                OutBuffer buf;
                mangleToBuffer(ds, buf);
                e = new StringExp(loc, buf.extractSlice());
            }

            return e.expressionSemantic(sc);
        }

        Dsymbol ds;
        switch (exp.e1.op)
        {
            case EXP.scope_:      return dotMangleof(exp.loc, sc, exp.e1.isScopeExp().sds, false);
            case EXP.overloadSet: return dotMangleof(exp.loc, sc, exp.e1.isOverExp().vars, false);
            case EXP.variable:
            {
                VarExp ve = exp.e1.isVarExp();
                return dotMangleof(exp.loc, sc, ve.var, ve.hasOverloads);
            }
            case EXP.dotVariable:
            {
                DotVarExp dve = exp.e1.isDotVarExp();
                return dotMangleof(exp.loc, sc, dve.var, dve.hasOverloads);
            }
            case EXP.template_:
            {
                TemplateExp te = exp.e1.isTemplateExp();
                return dotMangleof(exp.loc, sc, ds = te.fd ? te.fd.isDsymbol() : te.td, false);
            }

            default:
                break;
        }
    }

    if (exp.e1.isVarExp() && exp.e1.type.toBasetype().isTypeSArray() && exp.ident == Id.length)
    {
        // bypass checkPurity
        return exp.e1.type.dotExp(sc, exp.e1, exp.ident, cast(DotExpFlag) (exp.noderef * DotExpFlag.noDeref));
    }

    if (!exp.e1.isDotExp())
    {
        exp.e1 = resolvePropertiesX(sc, exp.e1);
    }

    if (auto te = exp.e1.isTupleExp())
    {
        if (exp.ident == Id.offsetof    ||
            exp.ident == Id.bitoffsetof ||
            exp.ident == Id.bitwidth)
        {
            /* 'distribute' the .offsetof to each of the tuple elements.
             */
            auto exps = new Expressions(te.exps.length);
            foreach (i, e; (*te.exps)[])
            {
                (*exps)[i] = new DotIdExp(e.loc, e, exp.ident);
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
        error(exp.loc, "template `%s` does not have property `%s`", exp.e1.toChars(), exp.ident.toChars());
        return ErrorExp.get();
    }
    if (!exp.e1.type)
    {
        error(exp.loc, "expression `%s` does not have property `%s`", exp.e1.toChars(), exp.ident.toChars());
        return ErrorExp.get();
    }

    return exp;
}

/******************************
 * Resolve properties, i.e. `e1.ident`, without seeing UFCS.
 * Params:
 *      exp = expression to resolve
 *      sc = context
 *      gag = do not emit error messages, just return `null`
 * Returns:
 *      resolved expression, null if error
 */
Expression dotIdSemanticProp(DotIdExp exp, Scope* sc, bool gag)
{
    //printf("dotIdSemanticProp('%s')\n", exp.toChars());

    //{ static int z; fflush(stdout); if (++z == 10) *(char*)0=0; }

    const cfile = sc.inCfile;

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
        SearchOptFlags flags = SearchOpt.localsOnly;
        /* Disable access to another module's private imports.
         * The check for 'is sds our current module' is because
         * the current module should have access to its own imports.
         */
        if (ie.sds.isModule() && ie.sds != sc._module)
            flags |= SearchOpt.ignorePrivateImports;
        if (sc.ignoresymbolvisibility)
            flags |= SearchOpt.ignoreVisibility;
        Dsymbol s = ie.sds.search(exp.loc, exp.ident, flags);
        /* Check for visibility before resolving aliases because public
         * aliases to private symbols are public.
         */
        if (s && !sc.ignoresymbolvisibility && !symbolIsVisible(sc._module, s))
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

            s.checkDeprecated(exp.loc, sc);
            if (auto d = s.isDeclaration())
                d.checkDisabled(exp.loc, sc);

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
                        error(exp.loc, "circular reference to %s `%s`", v.kind(), v.toPrettyChars());
                    else
                        error(exp.loc, "forward reference to %s `%s`", v.kind(), v.toPrettyChars());
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
                if (!functionSemantic(f))
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

            if (auto attr = s.isAttribDeclaration())
            {
                if (auto sm = ie.sds.search(exp.loc, exp.ident, flags))
                {
                    auto es = new DsymbolExp(exp.loc, sm);
                    return es;
                }
            }

            // BUG: handle other cases like in IdentifierExp::semantic()
            debug
            {
                printf("s = %p '%s', kind = '%s'\n", s, s.toChars(), s.kind());
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
            gag = false;
        }
        if (gag)
            return null;
        s = ie.sds.search_correct(exp.ident);
        if (s && symbolIsVisible(sc, s))
        {
            if (s.isPackage())
                error(exp.loc, "undefined identifier `%s` in %s `%s`, perhaps add `static import %s;`", exp.ident.toChars(), ie.sds.kind(), ie.sds.toPrettyChars(), s.toPrettyChars());
            else
                error(exp.loc, "undefined identifier `%s` in %s `%s`, did you mean %s `%s`?", exp.ident.toChars(), ie.sds.kind(), ie.sds.toPrettyChars(), s.kind(), s.toChars());
        }
        else
            error(exp.loc, "undefined identifier `%s` in %s `%s`", exp.ident.toChars(), ie.sds.kind(), ie.sds.toPrettyChars());
        return ErrorExp.get();
    }
    else if (t1b.ty == Tpointer && exp.e1.type.ty != Tenum &&
             !(
               exp.ident == Id.__sizeof ||
               exp.ident == Id.__xalignof ||
               !cfile &&
                (exp.ident == Id._mangleof ||
                 exp.ident == Id.offsetof ||
                 exp.ident == Id.bitoffsetof ||
                 exp.ident == Id.bitwidth ||
                 exp.ident == Id._init ||
                 exp.ident == Id.stringof)
              ))
    {
        Type t1bn = t1b.nextOf();
        if (gag)
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
        if (gag && t1bn.ty == Tvoid)
            return null;
        Expression e = new PtrExp(exp.loc, exp.e1);
        e = e.expressionSemantic(sc);
        const newFlag = cast(DotExpFlag) (gag * DotExpFlag.gag | exp.noderef * DotExpFlag.noDeref);
        return e.type.dotExp(sc, e, exp.ident, newFlag);
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
            gag = false;

        const flag = cast(DotExpFlag) (exp.noderef * DotExpFlag.noDeref | gag * DotExpFlag.gag);

        Expression e = dotExp(exp.e1.type, sc, exp.e1, exp.ident, flag);
        if (e)
        {
            e = e.expressionSemantic(sc);
        }
        return e;
    }
}

/**
 * Resolve `e1.ident!tiargs` without seeing UFCS.
 * Params:
 *     exp = the `DotTemplateInstanceExp` to resolve
 *     sc = the semantic scope
 *     gag = stop "not a property" error and return `null`.
 * Returns:
 *     `null` if error or not found, or the resolved expression.
 */
Expression dotTemplateSemanticProp(DotTemplateInstanceExp exp, Scope* sc, bool gag)
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

    Expression notTemplate()
    {
        error(exp.loc, "`%s` isn't a template", e.toChars());
        return errorExp();
    }

    if (e == die)
    {
        exp.e1 = die.e1; // take back
        Type t1b = exp.e1.type.toBasetype();
        if (t1b.ty == Tarray || t1b.ty == Tsarray || t1b.ty == Taarray || t1b.ty == Tnull || (t1b.isTypeBasic() && t1b.ty != Tvoid))
        {
            /* No built-in type has templatized properties, so do shortcut.
             * It is necessary in: 1024.max!"a < b"
             */
            if (gag)
                return null;
        }
        e = die.dotIdSemanticProp(sc, gag);
        if (gag)
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
                return notTemplate();
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

    return notTemplate();
}

MATCH matchType(FuncExp funcExp, Type to, Scope* sc, FuncExp* presult, ErrorSink eSink)
{
    auto loc = funcExp.loc;
    auto tok = funcExp.tok;
    auto td = funcExp.td;
    auto fd = funcExp.fd;
    auto type = funcExp.type;

    MATCH cannotInfer()
    {
        eSink.error(loc, "cannot infer parameter types from `%s`", to.toChars());
        return MATCH.nomatch;
    }

    //printf("FuncExp::matchType('%s'), to=%s\n", type ? type.toChars() : "null", to.toChars());
    if (presult)
        *presult = null;

    TypeFunction tof = null;
    if (to.ty == Tdelegate)
    {
        if (tok == TOK.function_)
        {
            eSink.error(loc, "cannot match function literal to delegate type `%s`", to.toChars());
            return MATCH.nomatch;
        }
        tof = cast(TypeFunction)to.nextOf();
    }
    else if (to.ty == Tpointer && (tof = to.nextOf().isTypeFunction()) !is null)
    {
        if (tok == TOK.delegate_)
        {
            eSink.error(loc, "cannot match delegate literal to function pointer type `%s`", to.toChars());
            return MATCH.nomatch;
        }
    }

    if (td)
    {
        if (!tof)
        {
            return cannotInfer();
        }

        // Parameter types inference from 'tof'
        assert(td._scope);
        TypeFunction tf = fd.type.isTypeFunction();
        //printf("\ttof = %s\n", tof.toChars());
        //printf("\ttf  = %s\n", tf.toChars());
        const dim = tf.parameterList.length;

        if (tof.parameterList.length != dim || tof.parameterList.varargs != tf.parameterList.varargs)
            return cannotInfer();

        auto tiargs = new Objects();
        tiargs.reserve(td.parameters.length);

        foreach (tp; *td.parameters)
        {
            size_t u = 0;
            foreach (i, p; tf.parameterList)
            {
                if (auto ti = p.type.isTypeIdentifier())
                    if (ti && ti.ident == tp.ident)
                        break;

                ++u;
            }
            assert(u < dim);
            Parameter pto = tof.parameterList[u];
            Type t = pto.type;
            if (t.ty == Terror)
                return cannotInfer();
            tf.parameterList[u].storageClass = tof.parameterList[u].storageClass;
            tiargs.push(t);
        }

        // Set target of return type inference
        if (!tf.next && tof.next)
            fd.treq = to;

        auto ti = new TemplateInstance(loc, td, tiargs);
        Expression ex = (new ScopeExp(loc, ti)).expressionSemantic(td._scope);

        // Reset inference target for the later re-semantic
        fd.treq = null;

        if (ex.op == EXP.error)
            return MATCH.nomatch;
        if (auto ef = ex.isFuncExp())
            return ef.matchType(to, sc, presult, eSink);
        else
            return cannotInfer();
    }

    if (!tof || !tof.next)
        return MATCH.nomatch;

    assert(type && type != Type.tvoid);
    if (fd.type.ty == Terror)
        return MATCH.nomatch;
    auto tfx = fd.type.isTypeFunction();
    bool convertMatch = (type.ty != to.ty);

    if (fd.inferRetType && tfx.next.implicitConvTo(tof.next) == MATCH.convert)
    {
        /* If return type is inferred and covariant return,
         * tweak return statements to required return type.
         *
         * interface I {}
         * class C : Object, I{}
         *
         * I delegate() dg = delegate() { return new class C(); }
         */
        convertMatch = true;

        auto tfy = new TypeFunction(tfx.parameterList, tof.next,
                    tfx.linkage, STC.undefined_);
        tfy.mod = tfx.mod;
        tfy.trust = tfx.trust;
        tfy.isNothrow = tfx.isNothrow;
        tfy.isNogc = tfx.isNogc;
        tfy.purity = tfx.purity;
        tfy.isProperty = tfx.isProperty;
        tfy.isRef = tfx.isRef;
        tfy.isInOutParam = tfx.isInOutParam;
        tfy.isInOutQual = tfx.isInOutQual;
        tfy.deco = tfy.merge().deco;

        tfx = tfy;
    }
    Type tx;
    if (tok == TOK.delegate_ ||
        tok == TOK.reserved && (type.ty == Tdelegate || type.ty == Tpointer && to.ty == Tdelegate))
    {
        // Allow conversion from implicit function pointer to delegate
        tx = new TypeDelegate(tfx);
        tx.deco = tx.merge().deco;
    }
    else
    {
        assert(tok == TOK.function_ || tok == TOK.reserved && type.ty == Tpointer || fd.errors);
        tx = tfx.pointerTo();
    }
    //printf("\ttx = %s, to = %s\n", tx.toChars(), to.toChars());

    MATCH m = tx.implicitConvTo(to);
    if (m > MATCH.nomatch)
    {
        // MATCH.exact:      exact type match
        // MATCH.constant:      covairiant type match (eg. attributes difference)
        // MATCH.convert:    context conversion
        m = convertMatch ? MATCH.convert : tx.equals(to) ? MATCH.exact : MATCH.constant;

        if (presult)
        {
            (*presult) = cast(FuncExp)funcExp.copy();
            (*presult).type = to;

            // https://issues.dlang.org/show_bug.cgi?id=12508
            // Tweak function body for covariant returns.
            (*presult).fd.modifyReturns(sc, tof.next);
        }
    }
    else if (!cast(ErrorSinkNull)eSink)
    {
        auto ts = toAutoQualChars(tx, to);
        eSink.error(loc, "cannot implicitly convert expression `%s` of type `%s` to `%s`",
            funcExp.toChars(), ts[0], ts[1]);
    }
    return m;
}

private bool checkSharedAccessBin(BinExp binExp, Scope* sc)
{
    const r1 = binExp.e1.checkSharedAccess(sc);
    const r2 = binExp.e2.checkSharedAccess(sc);
    return (r1 || r2);
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
    if (!sc ||
        !sc.previews.noSharedAccess ||
        sc.intypeof ||
        sc.ctfe)
    {
        return false;
    }
    else if (sc._module.ident == Id.atomic && sc._module.parent !is null)
    {
        // Allow core.internal.atomic, it is an compiler implementation for a given platform module.
        // It is then exposed by other modules such as core.atomic and core.stdc.atomic.
        // This is available as long as druntime is on the import path and the platform supports that operation.

        // https://issues.dlang.org/show_bug.cgi?id=24846

        Package parent = sc._module.parent.isPackage();
        if (parent !is null)
        {
            // This can be easily converted over to apply to core.atomic and core.internal.atomic
            if (parent.ident == Id.internal)
            {
                parent = parent.parent.isPackage();

                if (parent !is null && parent.ident == Id.core && parent.parent is null)
                   return false;
            }
        }
    }

    //printf("checkSharedAccess() `%s` returnRef: %d\n", e.toChars(), returnRef);

    bool check(Expression e, bool allowRef)
    {
        bool sharedError(Expression e)
        {
            // https://dlang.org/phobos/core_atomic.html
            error(e.loc, "direct access to shared `%s` is not allowed, see `core.atomic`", e.toChars());
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
            }
            if (!allowRef && e.var.type.isShared())
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

/****************************************
 * Resolve __FILE__, __LINE__, __MODULE__, __FUNCTION__, __PRETTY_FUNCTION__, __FILE_FULL_PATH__ to loc.
 */
Expression resolveLoc(Expression exp, const ref Loc loc, Scope* sc)
{
    // Don't replace the special keywords, while we are inside a default
    // argument. They are replaced later when copied to the call site.
    if (sc.inDefaultArg)
        return exp;

    exp.loc = loc;

    Expression visit(Expression exp)
    {
        if (auto binExp = exp.isBinExp())
        {
            binExp.e1 = binExp.e1.resolveLoc(loc, sc);
            binExp.e2 = binExp.e2.resolveLoc(loc, sc);
            return binExp;
        }
        if (auto unaExp = exp.isUnaExp())
        {
            unaExp.e1 = unaExp.e1.resolveLoc(loc, sc);
            return unaExp;
        }
        return exp;
    }

    Expression visitCond(CondExp exp)
    {
        exp.e1 = exp.e1.resolveLoc(loc, sc);
        exp.e2 = exp.e2.resolveLoc(loc, sc);
        exp.econd = exp.econd.resolveLoc(loc, sc);
        return exp;
    }

    Expression visitCat(CatExp exp)
    {
        exp.e1 = exp.e1.resolveLoc(loc, sc);
        exp.e2 = exp.e2.resolveLoc(loc, sc);
        if (exp.lowering)
            exp.lowering = exp.lowering.resolveLoc(loc, sc);
        return exp;
    }

    Expression visitStructLiteral(StructLiteralExp exp)
    {
        if (!exp.elements)
            return exp;

        foreach (ref element; *exp.elements)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        return exp;
    }

    Expression visitNew(NewExp exp)
    {
        if (exp.thisexp)
            exp.thisexp = exp.thisexp.resolveLoc(loc, sc);
        if (exp.argprefix)
            exp.argprefix = exp.argprefix.resolveLoc(loc, sc);
        if (exp.lowering)
            exp.lowering = exp.lowering.resolveLoc(loc, sc);

        if (!exp.arguments)
            return exp;

        foreach (ref element; *exp.arguments)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        return exp;
    }

    Expression visitCall(CallExp exp)
    {
        if (!exp.arguments)
            return exp;

        foreach (ref element; *exp.arguments)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        return exp;
    }

    Expression visitArray(ArrayExp exp)
    {
        exp.e1 = exp.e1.resolveLoc(loc, sc);

        if (!exp.arguments)
            return exp;

        foreach (ref element; *exp.arguments)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        return exp;
    }

    Expression visitSlice(SliceExp exp)
    {
        exp.e1 = exp.e1.resolveLoc(loc, sc);
        if (exp.lwr)
            exp.lwr = exp.lwr.resolveLoc(loc, sc);
        if (exp.upr)
            exp.upr = exp.upr.resolveLoc(loc, sc);

        return exp;
    }

    Expression visitInterval(IntervalExp exp)
    {
        exp.lwr = exp.lwr.resolveLoc(loc, sc);
        exp.upr = exp.upr.resolveLoc(loc, sc);

        return exp;
    }

    Expression visitArrayLiteral(ArrayLiteralExp exp)
    {
        if (exp.basis)
            exp.basis = exp.basis.resolveLoc(loc, sc);

        if (!exp.elements)
            return exp;

        foreach (ref element; *exp.elements)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        return exp;
    }

    Expression visitAssocArrayLiteral(AssocArrayLiteralExp exp)
    {
        foreach (ref element; *exp.keys)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        foreach (ref element; *exp.values)
        {
            if (element)
                element = element.resolveLoc(loc, sc);
        }

        return exp;
    }

    Expression visitFileInit(FileInitExp exp)
    {
        //printf("FileInitExp::resolve() %s\n", exp.toChars());
        const(char)* s;
        if (exp.op == EXP.fileFullPath)
            s = FileName.toAbsolute(loc.isValid() ? loc.filename : sc._module.srcfile.toChars());
        else
            s = loc.isValid() ? loc.filename : sc._module.ident.toChars();

        Expression e = new StringExp(loc, s.toDString());
        return e.expressionSemantic(sc);
    }

    Expression visitLineInit(LineInitExp exp)
    {
        Expression e = new IntegerExp(loc, loc.linnum, Type.tint32);
        return e.expressionSemantic(sc);
    }

    Expression visitModuleInit(ModuleInitExp exp)
    {
        const auto s = (sc.callsc ? sc.callsc : sc)._module.toPrettyChars().toDString();
        Expression e = new StringExp(loc, s);
        return e.expressionSemantic(sc);
    }

    Expression visitFuncInit(FuncInitExp exp)
    {
        const(char)* s;
        if (sc.callsc && sc.callsc.func)
            s = sc.callsc.func.Dsymbol.toPrettyChars();
        else if (sc.func)
            s = sc.func.Dsymbol.toPrettyChars();
        else
            s = "";
        Expression e = new StringExp(loc, s.toDString());
        return e.expressionSemantic(sc);
    }

    Expression visitPrettyFunc(PrettyFuncInitExp exp)
    {
        FuncDeclaration fd = (sc.callsc && sc.callsc.func)
                        ? sc.callsc.func
                        : sc.func;

        const(char)* s;
        if (fd)
        {
            const funcStr = fd.Dsymbol.toPrettyChars();
            OutBuffer buf;
            functionToBufferWithIdent(fd.type.isTypeFunction(), buf, funcStr, fd.isStatic);
            s = buf.extractChars();
        }
        else
        {
            s = "";
        }

        Expression e = new StringExp(loc, s.toDString());
        e = e.expressionSemantic(sc);
        e.type = Type.tstring;
        return e;
    }

    switch(exp.op)
    {
        default:                 return visit(exp);
        case EXP.structLiteral:  return visitStructLiteral(exp.isStructLiteralExp());
        case EXP.new_:           return visitNew(exp.isNewExp());
        case EXP.concatenate:    return visitCat(exp.isCatExp());
        case EXP.call:           return visitCall(exp.isCallExp());
        case EXP.question:       return visitCond(exp.isCondExp());
        case EXP.array:          return visitArray(exp.isArrayExp());
        case EXP.slice:          return visitSlice(exp.isSliceExp());
        case EXP.interval:       return visitInterval(exp.isIntervalExp());
        case EXP.arrayLiteral:   return visitArrayLiteral(exp.isArrayLiteralExp());
        case EXP.assocArrayLiteral: return visitAssocArrayLiteral(exp.isAssocArrayLiteralExp());
        case EXP.file:
        case EXP.fileFullPath:   return visitFileInit(exp.isFileInitExp());
        case EXP.line:           return visitLineInit(exp.isLineInitExp);
        case EXP.moduleString:   return visitModuleInit(exp.isModuleInitExp());
        case EXP.functionString: return visitFuncInit(exp.isFuncInitExp());
        case EXP.prettyFunction: return visitPrettyFunc(exp.isPrettyFuncInitExp());
    }
}

/************************************************
 * Destructors are attached to VarDeclarations.
 * Hence, if expression returns a temp that needs a destructor,
 * make sure and create a VarDeclaration for that temp.
 */
Expression addDtorHook(Expression e, Scope* sc)
{
    //printf("addDtorHook() %s\n", toChars(e));
    Expression visit(Expression exp)
    {
        return exp;
    }

    Expression visitStructLiteral(StructLiteralExp exp)
    {
        auto sd = exp.sd;
        /* If struct requires a destructor, rewrite as:
         *    (S tmp = S()),tmp
         * so that the destructor can be hung on tmp.
         */
        if (sd.dtor && sc.func)
        {
            /* Make an identifier for the temporary of the form:
             *   __sl%s%d, where %s is the struct name
             */
            char[10] buf = void;
            const prefix = "__sl";
            const ident = sd.ident.toString;
            const fullLen = prefix.length + ident.length;
            const len = fullLen < buf.length ? fullLen : buf.length;
            buf[0 .. prefix.length] = prefix;
            buf[prefix.length .. len] = ident[0 .. len - prefix.length];

            auto tmp = copyToTemp(0, buf[0 .. len], exp);
            Expression ae = new DeclarationExp(exp.loc, tmp);
            Expression e = new CommaExp(exp.loc, ae, new VarExp(exp.loc, tmp));
            e = e.expressionSemantic(sc);
            return e;
        }

        return exp;
    }

    Expression visitCall(CallExp exp)
    {
        auto e1 = exp.e1;
        auto type = exp.type;
        /* Only need to add dtor hook if it's a type that needs destruction.
         * Use same logic as VarDeclaration::callScopeDtor()
         */

        if (auto tf = e1.type.isTypeFunction())
        {
            if (tf.isRef)
                return exp;
        }

        Type tv = type.baseElemOf();
        if (auto ts = tv.isTypeStruct())
        {
            StructDeclaration sd = ts.sym;
            if (sd.dtor)
            {
                /* Type needs destruction, so declare a tmp
                 * which the back end will recognize and call dtor on
                 */
                auto tmp = copyToTemp(0, Id.__tmpfordtor.toString(), exp);
                auto de = new DeclarationExp(exp.loc, tmp);
                auto ve = new VarExp(exp.loc, tmp);
                Expression e = new CommaExp(exp.loc, de, ve);
                e = e.expressionSemantic(sc);
                return e;
            }
        }

        return exp;
    }

    Expression visitCast(CastExp exp)
    {
        if (exp.to.toBasetype().ty == Tvoid)        // look past the cast(void)
            exp.e1 = exp.e1.addDtorHook(sc);
        return exp;
    }

    Expression visitComma(CommaExp exp)
    {
        exp.e2 = exp.e2.addDtorHook(sc);
        return exp;
    }

    switch(e.op)
    {
        default: return visit(e);

        case EXP.structLiteral:    return visitStructLiteral(e.isStructLiteralExp());
        case EXP.call:             return visitCall(e.isCallExp());
        case EXP.cast_:            return visitCast(e.isCastExp());
        case EXP.comma:            return visitComma(e.isCommaExp());
    }
}

/*******************************
 * Try to convert an expression to be an lvalue.
 *
 * Give error if we're not an lvalue.
 * Params:
 *     _this = expression to convert
 *     sc = scope
 *     action = for error messages, what the lvalue is needed for (e.g. take address of for `&x`, modify for `x++`)
 * Returns: converted expression, or `ErrorExp` on error
*/
Expression toLvalue(Expression _this, Scope* sc, const(char)* action)
{
    return toLvalueImpl(_this, sc, action, _this);
}

// e = original un-lowered expression for error messages, in case of recursive calls
private Expression toLvalueImpl(Expression _this, Scope* sc, const(char)* action, Expression e)
{
    if (!action)
        action = "create lvalue of";

    assert(e);
    Expression visit(Expression _this)
    {
        // BinaryAssignExp does not have an EXP associated
        // so it's treated on the default path.
        // Lvalue-ness will be handled in glue :layer.
        if (_this.isBinAssignExp())
            return _this;
        if (!_this.loc.isValid())
            _this.loc = e.loc;

        if (e.op == EXP.type)
            error(_this.loc, "cannot %s type `%s`", action, e.type.toChars());
        else if (e.op == EXP.template_)
            error(_this.loc, "cannot %s template `%s`, perhaps instantiate it first", action, e.toChars());
        else
            error(_this.loc, "cannot %s expression `%s` because it is not an lvalue", action, e.toChars());

        return ErrorExp.get();
    }

    Expression visitInteger(IntegerExp _this)
    {
        if (!_this.loc.isValid())
            _this.loc = e.loc;
        error(e.loc, "cannot %s constant `%s`", action, e.toChars());
        return ErrorExp.get();
    }

    Expression visitThis(ThisExp _this)
    {
        if (_this.type.toBasetype().ty == Tclass)
        {
            // Class `this` is an rvalue; struct `this` is an lvalue.
            return visit(_this);
        }

        return _this;
    }

    Expression visitString(StringExp _this)
    {
        //printf("StringExp::toLvalue(%s) type = %s\n", _this.toChars(), _this.type ? _this.type.toChars() : NULL);
        return (_this.type && _this.type.toBasetype().ty == Tsarray) ? _this : visit(_this);
    }

    Expression visitStructLiteral(StructLiteralExp _this)
    {
        if (sc.inCfile)
            return _this;  // C struct literals are lvalues
        else
            return visit(_this);
    }

    Expression visitTemplate(TemplateExp _this)
    {
        if (!_this.fd)
            return visit(_this);

        assert(sc);
        return symbolToExp(_this.fd, _this.loc, sc, true);

    }

    Expression visitVar(VarExp _this)
    {
        auto var = _this.var;
        if (var.storage_class & STC.manifest)
        {
            error(_this.loc, "cannot %s manifest constant `%s`", action, var.toChars());
            return ErrorExp.get();
        }
        if (var.storage_class & STC.lazy_ && !_this.delegateWasExtracted)
        {
            error(_this.loc, "cannot %s lazy variable `%s`", action, var.toChars());
            return ErrorExp.get();
        }
        if (var.ident == Id.ctfe)
        {
            error(_this.loc, "cannot %s compiler-generated variable `__ctfe`", action);
            return ErrorExp.get();
        }
        if (var.ident == Id.dollar) // https://issues.dlang.org/show_bug.cgi?id=13574
        {
            error(_this.loc, "cannot %s operator `$`", action);
            return ErrorExp.get();
        }
        return _this;
    }

    Expression visitDotVar(DotVarExp _this)
    {
        auto e1 = _this.e1;
        auto var = _this.var;
        //printf("DotVarExp::toLvalue(%s)\n", toChars());
        if (sc && sc.inCfile)
        {
            /* C11 6.5.2.3-3: A postfix expression followed by the '.' or '->' operator
             * is an lvalue if the first expression is an lvalue.
             */
            if (!e1.isLvalue())
                return visit(_this);
        }
        if (!_this.isLvalue())
            return visit(_this);
        if (e1.op == EXP.this_ && sc.ctorflow.fieldinit.length && !(sc.ctorflow.callSuper & CSX.any_ctor))
        {
            if (VarDeclaration vd = var.isVarDeclaration())
            {
                auto ad = vd.isMember2();
                if (ad && ad.fields.length == sc.ctorflow.fieldinit.length)
                {
                    foreach (i, f; ad.fields)
                    {
                        if (f == vd)
                        {
                            if (!(sc.ctorflow.fieldinit[i].csx & CSX.this_ctor))
                            {
                                /* If the address of vd is taken, assume it is thereby initialized
                                 * https://issues.dlang.org/show_bug.cgi?id=15869
                                 */
                                modifyFieldVar(_this.loc, sc, vd, e1);
                            }
                            break;
                        }
                    }
                }
            }
        }
        return _this;
    }

    Expression visitCall(CallExp _this)
    {
        if (_this.isLvalue())
            return _this;
        return visit(_this);
    }

    Expression visitCast(CastExp _this)
    {
        if (sc && sc.inCfile)
        {
            /* C11 6.5.4-5: A cast does not yield an lvalue.
             */
            return visit(_this);
        }
        if (_this.isLvalue())
        {
            with (_this)
            if (!trusted && !e1.type.pointerTo().implicitConvTo(to.pointerTo()))
                sc.setUnsafePreview(FeatureState.default_, false, loc,
                    "using the result of a cast from `%s` to `%s` as an lvalue",
                    e1.type, to);

            return _this;
        }
        return visit(_this);
    }

    Expression visitVectorArray(VectorArrayExp _this)
    {
        _this.e1 = _this.e1.toLvalueImpl(sc, action, e);
        return _this;
    }

    Expression visitSlice(SliceExp _this)
    {
        //printf("SliceExp::toLvalue(%s) _this.type = %s\n", _this.toChars(), _this.type ? _this.type.toChars() : NULL);
        return (_this.type && _this.type.toBasetype().ty == Tsarray) ? _this : visit(_this);
    }

    Expression visitArray(ArrayExp _this)
    {
        if (_this.type && _this.type.toBasetype().ty == Tvoid)
            error(_this.loc, "`void`s have no value");
        return _this;
    }

    Expression visitComma(CommaExp _this)
    {
        _this.e2 = _this.e2.toLvalue(sc, action);
        return _this;
    }

    Expression visitDelegatePointer(DelegatePtrExp _this)
    {
        _this.e1 = _this.e1.toLvalueImpl(sc, action, e);
        return _this;
    }

    Expression visitDelegateFuncptr(DelegateFuncptrExp _this)
    {
        _this.e1 = _this.e1.toLvalueImpl(sc, action, e);
        return _this;
    }

    Expression visitIndex(IndexExp _this)
    {
        if (_this.isLvalue())
            return _this;
        return visit(_this);
    }

    Expression visitAssign(AssignExp _this)
    {
        if (_this.e1.op == EXP.slice || _this.e1.op == EXP.arrayLength)
        {
            return visit(_this);
        }

        /* In front-end level, AssignExp should make an lvalue of e1.
         * Taking the address of e1 will be handled in low level layer,
         * so this function does nothing.
         */
        return _this;
    }

    Expression visitCond(CondExp _this)
    {
        // convert (econd ? e1 : e2) to *(econd ? &e1 : &e2)
        CondExp e = cast(CondExp)(_this.copy());
        e.e1 = _this.e1.toLvalue(sc, action).addressOf();
        e.e2 = _this.e2.toLvalue(sc, action).addressOf();
        e.type = _this.type.pointerTo();
        return new PtrExp(_this.loc, e, _this.type);

    }

    switch(_this.op)
    {
        default:                          return visit(_this);

        case EXP.int64:                   return visitInteger(_this.isIntegerExp());
        case EXP.error:                   return _this;
        case EXP.identifier:              return _this;
        case EXP.dSymbol:                 return _this;
        case EXP.this_:                   return visitThis(_this.isThisExp());
        case EXP.super_:                  return visitThis(_this.isSuperExp());
        case EXP.string_:                 return visitString(_this.isStringExp());
        case EXP.structLiteral:           return visitStructLiteral(_this.isStructLiteralExp());
        case EXP.template_:               return visitTemplate(_this.isTemplateExp());
        case EXP.variable:                return visitVar(_this.isVarExp());
        case EXP.overloadSet:             return _this;
        case EXP.dotVariable:             return visitDotVar(_this.isDotVarExp());
        case EXP.call:                    return visitCall(_this.isCallExp());
        case EXP.star:                    return _this;
        case EXP.cast_:                   return visitCast(_this.isCastExp());
        case EXP.vectorArray:             return visitVectorArray(_this.isVectorArrayExp());
        case EXP.slice:                   return visitSlice(_this.isSliceExp());
        case EXP.array:                   return visitArray(_this.isArrayExp());
        case EXP.comma:                   return visitComma(_this.isCommaExp());
        case EXP.delegatePointer:         return visitDelegatePointer(_this.isDelegatePtrExp());
        case EXP.delegateFunctionPointer: return visitDelegateFuncptr(_this.isDelegateFuncptrExp());
        case EXP.index:                   return visitIndex(_this.isIndexExp());
        case EXP.construct:               return visitAssign(_this.isConstructExp());
        case EXP.loweredAssignExp:        return visitAssign(_this.isLoweredAssignExp());
        case EXP.blit:                    return visitAssign(_this.isBlitExp());
        case EXP.assign:                  return visitAssign(_this.isAssignExp());
        case EXP.question:                return visitCond(_this.isCondExp());
    }
}

/***************************************
 * Parameters:
 *      sc:     scope
 *      flag:   1: do not issue error message for invalid modification
                2: the exp is a DotVarExp and a subfield of the leftmost
                   variable is modified
 * Returns:
 *      Whether the type is modifiable
 */
Modifiable checkModifiable(Expression exp, Scope* sc, ModifyFlags flag = ModifyFlags.none)
{
    switch(exp.op)
    {
        case EXP.variable:
            auto varExp = cast(VarExp)exp;

            //printf("VarExp::checkModifiable %s", varExp.toChars());
            assert(varExp.type);
            return varExp.var.checkModify(varExp.loc, sc, null, flag);

        case EXP.dotVariable:
            auto dotVarExp = cast(DotVarExp)exp;

            //printf("DotVarExp::checkModifiable %s %s\n", dotVarExp.toChars(), dotVarExp.type.toChars());
            if (dotVarExp.e1.op == EXP.this_)
                return dotVarExp.var.checkModify(dotVarExp.loc, sc, dotVarExp.e1, flag);

            /* https://issues.dlang.org/show_bug.cgi?id=12764
             * If inside a constructor and an expression of type `this.field.var`
             * is encountered, where `field` is a struct declaration with
             * default construction disabled, we must make sure that
             * assigning to `var` does not imply that `field` was initialized
             */
            if (sc.func && sc.func.isCtorDeclaration())
            {
                // if inside a constructor scope and e1 of this DotVarExp
                // is another DotVarExp, then check if the leftmost expression is a `this` identifier
                if (auto dve = dotVarExp.e1.isDotVarExp())
                {
                    // Iterate the chain of DotVarExp to find `this`
                    // Keep track whether access to fields was limited to union members
                    // s.t. one can initialize an entire struct inside nested unions
                    // (but not its members)
                    bool onlyUnion = true;
                    while (true)
                    {
                        auto v = dve.var.isVarDeclaration();
                        assert(v);

                        // Accessing union member?
                        auto t = v.type.isTypeStruct();
                        if (!t || !t.sym.isUnionDeclaration())
                            onlyUnion = false;

                        // Another DotVarExp left?
                        if (!dve.e1 || dve.e1.op != EXP.dotVariable)
                            break;

                        dve = cast(DotVarExp) dve.e1;
                    }

                    if (dve.e1.op == EXP.this_)
                    {
                        scope v = dve.var.isVarDeclaration();
                        /* if v is a struct member field with no initializer, no default construction
                         * and v wasn't intialized before
                         */
                        if (v && v.isField() && !v._init && !v.ctorinit)
                        {
                            if (auto ts = v.type.isTypeStruct())
                            {
                                if (ts.sym.noDefaultCtor)
                                {
                                    /* checkModify will consider that this is an initialization
                                     * of v while it is actually an assignment of a field of v
                                     */
                                    scope modifyLevel = v.checkModify(dotVarExp.loc, sc, dve.e1, !onlyUnion ? (flag | ModifyFlags.fieldAssign) : flag);
                                    if (modifyLevel == Modifiable.initialization)
                                    {
                                        // https://issues.dlang.org/show_bug.cgi?id=22118
                                        // v is a union type field that was assigned
                                        // a variable, therefore it counts as initialization
                                        if (v.ctorinit)
                                            return Modifiable.initialization;

                                        return Modifiable.yes;
                                    }
                                    return modifyLevel;
                                }
                            }
                        }
                    }
                }
            }

            //printf("\te1 = %s\n", e1.toChars());
            return dotVarExp.e1.checkModifiable(sc, flag);

        case EXP.star:
            auto ptrExp = cast(PtrExp)exp;
            if (auto se = ptrExp.e1.isSymOffExp())
            {
                return se.var.checkModify(ptrExp.loc, sc, null, flag);
            }
            else if (auto ae = ptrExp.e1.isAddrExp())
            {
                return ae.e1.checkModifiable(sc, flag);
            }
            return Modifiable.yes;

        case EXP.slice:
            auto sliceExp = cast(SliceExp)exp;

            //printf("SliceExp::checkModifiable %s\n", sliceExp.toChars());
            auto e1 = sliceExp.e1;
            if (e1.type.ty == Tsarray || (e1.op == EXP.index && e1.type.ty != Tarray) || e1.op == EXP.slice)
            {
                return e1.checkModifiable(sc, flag);
            }
            return Modifiable.yes;

        case EXP.comma:
            return (cast(CommaExp)exp).e2.checkModifiable(sc, flag);

        case EXP.index:
            auto indexExp = cast(IndexExp)exp;
            auto e1 = indexExp.e1;
            if (e1.type.ty == Tsarray ||
                e1.type.ty == Taarray ||
                (e1.op == EXP.index && e1.type.ty != Tarray) ||
                e1.op == EXP.slice)
            {
                return e1.checkModifiable(sc, flag);
            }
            return Modifiable.yes;

        case EXP.question:
            auto condExp = cast(CondExp)exp;
            if (condExp.e1.checkModifiable(sc, flag) != Modifiable.no
                && condExp.e2.checkModifiable(sc, flag) != Modifiable.no)
                return Modifiable.yes;
            return Modifiable.no;

        default:
            return exp.type ? Modifiable.yes : Modifiable.no; // default modifiable
    }
}

/**
 * Similar to `toLvalue`, but also enforce it is mutable or raise an error.
 * Params:
 *     _this = Expression to convert
 *     sc = scope
 * Returns: `_this` converted to an lvalue, or an `ErrorExp`
 */
Expression modifiableLvalue(Expression _this, Scope* sc)
{
    return modifiableLvalueImpl(_this, sc, _this);
}

// e = original / un-lowered expression to print in error messages
private Expression modifiableLvalueImpl(Expression _this, Scope* sc, Expression e)
{
    assert(e);
    Expression visit(Expression exp)
    {
        //printf("Expression::modifiableLvalue() %s, type = %s\n", exp.toChars(), exp.type.toChars());
        // See if this expression is a modifiable lvalue (i.e. not const)
        if (exp.isBinAssignExp())
            return exp.toLvalue(sc, "modify");

        auto type = exp.type;
        if (checkModifiable(exp, sc) == Modifiable.yes)
        {
            assert(type);
            if (!type.isMutable())
            {
                if (auto dve = exp.isDotVarExp())
                {
                    if (isNeedThisScope(sc, dve.var))
                        for (Dsymbol s = sc.func; s; s = s.toParentLocal())
                    {
                        FuncDeclaration ff = s.isFuncDeclaration();
                        if (!ff)
                            break;
                        if (!ff.type.isMutable)
                        {
                            error(exp.loc, "cannot modify `%s` in `%s` function", exp.toChars(), MODtoChars(type.mod));
                            return ErrorExp.get();
                        }
                    }
                }
                error(exp.loc, "cannot modify `%s` expression `%s`", MODtoChars(type.mod), exp.toChars());
                return ErrorExp.get();
            }
            else if (!type.isAssignable())
            {
                error(exp.loc, "cannot modify struct instance `%s` of type `%s` because it contains `const` or `immutable` members",
                    exp.toChars(), type.toChars());
                return ErrorExp.get();
            }
        }
        return exp.toLvalueImpl(sc, "modify", e);
    }

    Expression visitString(StringExp exp)
    {
        error(exp.loc, "cannot modify string literal `%s`", exp.toChars());
        return ErrorExp.get();
    }

    Expression visitVar(VarExp exp)
    {
        //printf("VarExp::modifiableLvalue('%s')\n", exp.var.toChars());
        if (exp.var.storage_class & STC.manifest)
        {
            error(exp.loc, "cannot modify manifest constant `%s`", exp.toChars());
            return ErrorExp.get();
        }
        // See if this expression is a modifiable lvalue (i.e. not const)
        return visit(exp);
    }

    Expression visitPtr(PtrExp exp)
    {
        //printf("PtrExp::modifiableLvalue() %s, type %s\n", exp.toChars(), exp.type.toChars());
        Declaration var;
        auto e1 = exp.e1;
        if (auto se = e1.isSymOffExp())
            var = se.var;
        else if (auto ve = e1.isVarExp())
            var = ve.var;
        if (var && var.type.isFunction_Delegate_PtrToFunction())
        {
            if (var.type.isTypeFunction())
                error(exp.loc, "function `%s` is not an lvalue and cannot be modified", var.toChars());
            else
                error(exp.loc, "function pointed to by `%s` is not an lvalue and cannot be modified", var.toChars());
            return ErrorExp.get();
        }
        return visit(exp);
    }

    Expression visitSlice(SliceExp exp)
    {
        error(exp.loc, "slice expression `%s` is not a modifiable lvalue", exp.toChars());
        return exp;
    }

    Expression visitComma(CommaExp exp)
    {
        exp.e2 = exp.e2.modifiableLvalueImpl(sc, e);
        return exp;
    }

    Expression visitDelegatePtr(DelegatePtrExp exp)
    {
        if (sc.setUnsafe(false, exp.loc, "modifying delegate pointer `%s`", exp))
        {
            return ErrorExp.get();
        }
        return visit(exp);
    }

    Expression visitDelegateFuncptr(DelegateFuncptrExp exp)
    {
        if (sc.setUnsafe(false, exp.loc, "modifying delegate function pointer `%s`", exp))
        {
            return ErrorExp.get();
        }
        return visit(exp);
    }

    Expression visitIndex(IndexExp exp)
    {
        //printf("IndexExp::modifiableLvalue(%s)\n", exp.toChars());
        Expression ex = exp.markSettingAAElem();
        if (ex.op == EXP.error)
            return ex;

        return visit(exp);
    }

    Expression visitCond(CondExp exp)
    {
        if (!exp.e1.isLvalue() && !exp.e2.isLvalue())
        {
            error(exp.loc, "conditional expression `%s` is not a modifiable lvalue", exp.toChars());
            return ErrorExp.get();
        }
        exp.e1 = exp.e1.modifiableLvalue(sc);
        exp.e2 = exp.e2.modifiableLvalue(sc);
        return exp.toLvalue(sc, "modify");
    }

    switch(_this.op)
    {
        default:                          return visit(_this);
        case EXP.string_:                 return visitString(_this.isStringExp());
        case EXP.variable:                return visitVar(_this.isVarExp());
        case EXP.star:                    return visitPtr(_this.isPtrExp());
        case EXP.slice:                   return visitSlice(_this.isSliceExp());
        case EXP.comma:                   return visitComma(_this.isCommaExp());
        case EXP.delegatePointer:         return visitDelegatePtr(_this.isDelegatePtrExp());
        case EXP.delegateFunctionPointer: return visitDelegateFuncptr(_this.isDelegateFuncptrExp());
        case EXP.index:                   return visitIndex(_this.isIndexExp());
        case EXP.question:                return visitCond(_this.isCondExp());
    }
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
private bool checkAddressVar(Scope* sc, Expression exp, VarDeclaration v)
{
    //printf("checkAddressVar(exp: %s, v: %s)\n", exp.toChars(), v.toChars());
    if (v is null)
        return true;

    if (!v.canTakeAddressOf())
    {
        error(exp.loc, "cannot take address of `%s`", exp.toChars());
        return false;
    }
    if (sc.func && !sc.intypeof && !v.isDataseg())
    {
        if (sc.useDIP1000 != FeatureState.enabled &&
            !(v.storage_class & STC.temp) &&
            sc.setUnsafe(false, exp.loc, "taking the address of stack-allocated local variable `%s`", v))
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
                    error(e.loc, "cannot take address of bit-field `%s`", bf.toChars());
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
                if (sc.inCfile)
                {
                    // C11 6.5.3.2: A variable that has its address taken cannot be
                    // stored in a register.
                    // C11 6.3.2.1: An array that has its address computed with `[]`
                    // or cast to an lvalue pointer cannot be stored in a register.
                    if (ex.isVarExp().var.storage_class & STC.register)
                    {
                        if (e.isIndexExp())
                            error(e.loc, "cannot index through register variable `%s`", ex.toChars());
                        else
                            error(e.loc, "cannot take address of register variable `%s`", ex.toChars());
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
    bool error = f.checkDisabled(exp.loc, sc);
    error |= f.checkDeprecated(exp.loc, sc);
    error |= f.checkPurity(exp.loc, sc);
    error |= f.checkSafety(exp.loc, sc);
    error |= f.checkNogc(exp.loc, sc);
    return error;
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
            error(e1.loc, "need `this` of type `%s` to access member `%s` from static function `%s`", ad.toChars(), var.toChars(), f.toChars());
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
private VarDeclaration makeThis2Argument(const ref Loc loc, Scope* sc, FuncDeclaration fd)
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
    Dsymbol pscopesym;
    auto rootSymbol = sc.search(loc, Id.empty, pscopesym);
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
                    "field `%s.%s` assigning to misaligned pointers", sd, v)))
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

        (*elements)[i] = doCopyOrMove(sc, e, null, false);
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
    {
        em.depdecl._scope = sc;
        em.depdecl._scope.setNoFree();
    }
    em.checkDeprecated(loc, sc);

    if (em.errors)
        return ErrorExp.get();
    Expression e = new VarExp(loc, em);
    e = e.expressionSemantic(sc);
    if (!sc.inCfile && em.isCsymbol())
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
            error(exp.loc, "`delete` does not give a boolean result");
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
        case EXP.loweredAssignExp:
            if (sc.inCfile)
                return exp;
            // Things like:
            //  if (a = b) ...
            // are usually mistakes.
            error(exp.loc, "assignment cannot be used as a condition, perhaps `==` was meant?");
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
                    error(exp.loc, "expression `%s` of type `%s` does not have a boolean value",
                              exp.toChars(), t.toChars());
                return ErrorExp.get();
            }
            return e;
    }
}

/********************************************
 * Semantically analyze and then evaluate a static condition at compile time.
 * This is special because short circuit operators &&, || and ?: at the top
 * level are not semantically analyzed if the result of the expression is not
 * necessary.
 * Params:
 *      sc  = instantiating scope
 *      original = original expression, for error messages
 *      e =  resulting expression
 *      errors = set to `true` if errors occurred
 *      negatives = array to store negative clauses
 * Returns:
 *      true if evaluates to true
 */
bool evalStaticCondition(Scope* sc, Expression original, Expression e, out bool errors, Expressions* negatives = null)
{
    if (negatives)
        negatives.setDim(0);

    bool impl(Expression e)
    {
        if (e.isNotExp())
        {
            NotExp ne = cast(NotExp)e;
            return !impl(ne.e1);
        }

        if (e.op == EXP.andAnd || e.op == EXP.orOr)
        {
            LogicalExp aae = cast(LogicalExp)e;
            bool result = impl(aae.e1);
            if (errors)
                return false;
            if (e.op == EXP.andAnd)
            {
                if (!result)
                    return false;
            }
            else
            {
                if (result)
                    return true;
            }
            result = impl(aae.e2);
            return !errors && result;
        }

        if (e.op == EXP.question)
        {
            CondExp ce = cast(CondExp)e;
            bool result = impl(ce.econd);
            if (errors)
                return false;
            Expression leg = result ? ce.e1 : ce.e2;
            result = impl(leg);
            return !errors && result;
        }

        Expression before = e;
        const uint nerrors = global.errors;

        sc = sc.startCTFE();
        sc.condition = true;

        e = e.expressionSemantic(sc);
        e = resolveProperties(sc, e);
        e = e.toBoolean(sc);

        sc = sc.endCTFE();
        e = e.optimize(WANTvalue);

        if (nerrors != global.errors ||
            e.isErrorExp() ||
            e.type.toBasetype() == Type.terror)
        {
            errors = true;
            return false;
        }

        e = e.ctfeInterpret();

        const opt = e.toBool();
        if (opt.isEmpty())
        {
            if (!e.type.isTypeError())
                error(e.loc, "expression `%s` is not constant", e.toChars());
            errors = true;
            return false;
        }

        if (negatives && !opt.get())
            negatives.push(before);
        return opt.get();
    }
    return impl(e);
}

/************************************
 * Check to see the aggregate type is nested and its context pointer is
 * accessible from the current scope.
 * Returns true if error occurs.
 */
bool checkFrameAccess(Loc loc, Scope* sc, AggregateDeclaration ad, size_t iStart = 0)
{
    Dsymbol sparent = ad.toParentLocal();
    Dsymbol sparent2 = ad.toParent2();
    Dsymbol s = sc.func;
    if (ad.isNested() && s)
    {
        //printf("ad = %p %s [%s], parent:%p\n", ad, ad.toChars(), ad.loc.toChars(), ad.parent);
        //printf("sparent = %p %s [%s], parent: %s\n", sparent, sparent.toChars(), sparent.loc.toChars(), sparent.parent,toChars());
        //printf("sparent2 = %p %s [%s], parent: %s\n", sparent2, sparent2.toChars(), sparent2.loc.toChars(), sparent2.parent,toChars());
        if (!ensureStaticLinkTo(s, sparent) || sparent != sparent2 && !ensureStaticLinkTo(s, sparent2))
        {
            error(loc, "cannot access frame pointer of `%s`", ad.toPrettyChars());
            return true;
        }
    }

    bool result = false;
    for (size_t i = iStart; i < ad.fields.length; i++)
    {
        VarDeclaration vd = ad.fields[i];
        Type tb = vd.type.baseElemOf();
        if (tb.ty == Tstruct)
        {
            result |= checkFrameAccess(loc, sc, (cast(TypeStruct)tb).sym);
        }
    }
    return result;
}

/// Return value for `checkModifiable`
enum Modifiable
{
    /// Not modifiable
    no,
    /// Modifiable (the type is mutable)
    yes,
    /// Modifiable because it is initialization
    initialization,
}

/**
 * Specifies how the checkModify deals with certain situations
 */
enum ModifyFlags
{
    /// Issue error messages on invalid modifications of the variable
    none,
    /// No errors are emitted for invalid modifications
    noError = 0x1,
    /// The modification occurs for a subfield of the current variable
    fieldAssign = 0x2,
}

/*************************************
 * Check to see if declaration can be modified in this context (sc).
 * Issue error if not.
 * Params:
 *  loc  = location for error messages
 *  e1   = `null` or `this` expression when this declaration is a field
 *  sc   = context
 *  flag = if the first bit is set it means do not issue error message for
 *         invalid modification; if the second bit is set, it means that
           this declaration is a field and a subfield of it is modified.
 * Returns:
 *  Modifiable.yes or Modifiable.initialization
 */
private Modifiable checkModify(Declaration d, Loc loc, Scope* sc, Expression e1, ModifyFlags flag)
{
    VarDeclaration v = d.isVarDeclaration();
    if (v && v.canassign)
        return Modifiable.initialization;

    if (d.isParameter() || d.isResult())
    {
        for (Scope* scx = sc; scx; scx = scx.enclosing)
        {
            if (scx.func == d.parent && scx.contract != Contract.none)
            {
                const(char)* s = d.isParameter() && d.parent.ident != Id.ensure ? "parameter" : "result";
                if (!(flag & ModifyFlags.noError))
                    error(loc, "%s `%s` cannot modify %s `%s` in contract", d.kind, d.toPrettyChars, s, d.toChars());
                return Modifiable.initialization; // do not report type related errors
            }
        }
    }

    if (e1 && e1.op == EXP.this_ && d.isField())
    {
        VarDeclaration vthis = e1.isThisExp().var;
        for (Scope* scx = sc; scx; scx = scx.enclosing)
        {
            if (scx.func == vthis.parent && scx.contract != Contract.none)
            {
                if (!(flag & ModifyFlags.noError))
                    error(loc, "%s `%s` cannot modify parameter `this` in contract", d.kind, d.toPrettyChars);
                return Modifiable.initialization; // do not report type related errors
            }
        }
    }

    if (v && (v.isCtorinit() || d.isField()))
    {
        // It's only modifiable if inside the right constructor
        if ((d.storage_class & (STC.foreach_ | STC.ref_)) == (STC.foreach_ | STC.ref_))
            return Modifiable.initialization;
        if (flag & ModifyFlags.fieldAssign)
            return Modifiable.yes;
        return modifyFieldVar(loc, sc, v, e1) ? Modifiable.initialization : Modifiable.yes;
    }
    return Modifiable.yes;
}

/***********************************************
 * Mark variable v as modified if it is inside a constructor that var
 * is a field in.
 * Also used to allow immutable globals to be initialized inside a static constructor.
 * Returns:
 *    true if it's an initialization of v
 */
private bool modifyFieldVar(Loc loc, Scope* sc, VarDeclaration var, Expression e1)
{
    //printf("modifyFieldVar(var = %s)\n", var.toChars());
    Dsymbol s = sc.func;
    while (1)
    {
        FuncDeclaration fd = null;
        if (s)
            fd = s.isFuncDeclaration();
        if (fd &&
            ((fd.isCtorDeclaration() && var.isField()) ||
             ((fd.isStaticCtorDeclaration() || fd.isCrtCtor) && !var.isField())) &&
            fd.toParentDecl() == var.toParent2() &&
            (!e1 || e1.op == EXP.this_))
        {
            bool result = true;

            var.ctorinit = true;
            //printf("setting ctorinit\n");

            if (var.isField() && sc.ctorflow.fieldinit.length && !sc.intypeof)
            {
                assert(e1);
                auto mustInit = ((var.storage_class & STC.nodefaultctor) != 0 ||
                                 var.type.needsNested());

                const dim = sc.ctorflow.fieldinit.length;
                auto ad = fd.isMemberDecl();
                assert(ad);
                size_t i;
                for (i = 0; i < dim; i++) // same as findFieldIndexByName in ctfeexp.c ?
                {
                    if (ad.fields[i] == var)
                        break;
                }
                assert(i < dim);
                auto fieldInit = &sc.ctorflow.fieldinit[i];
                const fi = fieldInit.csx;

                if (fi & CSX.this_ctor)
                {
                    if (var.type.isMutable() && e1.type.isMutable())
                        result = false;
                    else
                    {
                        const(char)* modStr = !var.type.isMutable() ? MODtoChars(var.type.mod) : MODtoChars(e1.type.mod);
                        .error(loc, "%s field `%s` initialized multiple times", modStr, var.toChars());
                        .errorSupplemental(fieldInit.loc, "Previous initialization is here.");
                    }
                }
                else if (sc.inLoop || (fi & CSX.label))
                {
                    if (!mustInit && var.type.isMutable() && e1.type.isMutable())
                        result = false;
                    else
                    {
                        const(char)* modStr = !var.type.isMutable() ? MODtoChars(var.type.mod) : MODtoChars(e1.type.mod);
                        .error(loc, "%s field `%s` initialization is not allowed in loops or after labels", modStr, var.toChars());
                    }
                }

                fieldInit.csx |= CSX.this_ctor;
                fieldInit.loc = e1.loc;
                if (var.overlapped) // https://issues.dlang.org/show_bug.cgi?id=15258
                {
                    foreach (j, v; ad.fields)
                    {
                        if (v is var || !var.isOverlappedWith(v))
                            continue;
                        v.ctorinit = true;
                        sc.ctorflow.fieldinit[j].csx = CSX.this_ctor;
                    }
                }
            }
            else if (fd != sc.func)
            {
                if (var.type.isMutable())
                    result = false;
                else if (sc.func.fes)
                {
                    const(char)* p = var.isField() ? "field" : var.kind();
                    .error(loc, "%s %s `%s` initialization is not allowed in foreach loop",
                        MODtoChars(var.type.mod), p, var.toChars());
                }
                else
                {
                    const(char)* p = var.isField() ? "field" : var.kind();
                    .error(loc, "%s %s `%s` initialization is not allowed in nested function `%s`",
                        MODtoChars(var.type.mod), p, var.toChars(), sc.func.toChars());
                }
            }
            else if (fd.isStaticCtorDeclaration() && !fd.isSharedStaticCtorDeclaration() &&
                     var.type.isImmutable())
            {
                .error(loc, "%s %s `%s` initialization is not allowed in `static this`",
                    MODtoChars(var.type.mod), var.kind(), var.toChars());
                errorSupplemental(loc, "Use `shared static this` instead.");
            }
            else if (fd.isStaticCtorDeclaration() && !fd.isSharedStaticCtorDeclaration() &&
                    var.type.isConst())
            {
                // @@@DEPRECATED_2.116@@@
                // Turn this into an error, merging with the branch above
                .deprecation(loc, "%s %s `%s` initialization is not allowed in `static this`",
                    MODtoChars(var.type.mod), var.kind(), var.toChars());
                deprecationSupplemental(loc, "Use `shared static this` instead.");
            }
            return result;
        }
        else
        {
            if (s)
            {
                s = s.toParentP(var.toParent2());
                continue;
            }
        }
        break;
    }
    return false;
}

/***************************************
 * Request additional semantic analysis for TypeInfo generation.
 * Params:
 *      sc = context
 *      t = type that TypeInfo is being generated for
 */
void semanticTypeInfo(Scope* sc, Type t)
{
    if (sc)
    {
        if (sc.intypeof)
            return;
        if (!sc.needsCodegen())
            return;
    }

    if (!t)
        return;

    void visitVector(TypeVector t)
    {
        semanticTypeInfo(sc, t.basetype);
    }

    void visitAArray(TypeAArray t)
    {
        semanticTypeInfo(sc, t.index);
        semanticTypeInfo(sc, t.next);
    }

    void visitStruct(TypeStruct t)
    {
        //printf("semanticTypeInfo.visit(TypeStruct = %s)\n", t.toChars());
        StructDeclaration sd = t.sym;

        /* Step 1: create TypeInfoDeclaration
         */
        if (!sc) // inline may request TypeInfo.
        {
            Scope scx;
            scx.eSink = global.errorSink;
            scx._module = sd.getModule();
            if (global.params.useTypeInfo)
            {
                getTypeInfoType(sd.loc, t, &scx);
                sd.requestTypeInfo = true;
            }
        }
        else if (!sc.minst)
        {
            // don't yet have to generate TypeInfo instance if
            // the typeid(T) expression exists in speculative scope.
        }
        else
        {
            getTypeInfoType(sd.loc, t, sc);
            sd.requestTypeInfo = true;

            // https://issues.dlang.org/show_bug.cgi?id=15149
            // if the typeid operand type comes from a
            // result of auto function, it may be yet speculative.
            // unSpeculative(sc, sd);
        }

        /* Step 2: If the TypeInfo generation requires sd.semantic3, run it later.
         * This should be done even if typeid(T) exists in speculative scope.
         * Because it may appear later in non-speculative scope.
         */
        if (!sd.members)
            return; // opaque struct
        if (!sd.xeq && !sd.xcmp && !sd.postblit && !sd.tidtor && !sd.xhash && !search_toString(sd))
            return; // none of TypeInfo-specific members

        // If the struct is in a non-root module, run semantic3 to get
        // correct symbols for the member function.
        if (sd.semanticRun >= PASS.semantic3)
        {
            // semantic3 is already done
        }
        else if (TemplateInstance ti = sd.isInstantiated())
        {
            if (ti.minst && !ti.minst.isRoot())
                Module.addDeferredSemantic3(sd);
        }
        else
        {
            if (sd.inNonRoot())
            {
                //printf("deferred sem3 for TypeInfo - sd = %s, inNonRoot = %d\n", sd.toChars(), sd.inNonRoot());
                Module.addDeferredSemantic3(sd);
            }
        }
    }

    void visitTuple(TypeTuple t)
    {
        if (t.arguments)
        {
            foreach (arg; *t.arguments)
            {
                semanticTypeInfo(sc, arg.type);
            }
        }
    }

    /* Note structural similarity of this Type walker to that in isSpeculativeType()
     */

    Type tb = t.toBasetype();
    switch (tb.ty)
    {
        case Tvector:   visitVector(tb.isTypeVector()); break;
        case Taarray:   visitAArray(tb.isTypeAArray()); break;
        case Tstruct:   visitStruct(tb.isTypeStruct()); break;
        case Ttuple:    visitTuple (tb.isTypeTuple());  break;

        case Tclass:
        case Tenum:     break;

        default:        semanticTypeInfo(sc, tb.nextOf()); break;
    }
}

/**
 * Issue an error if an attempt to call a disabled method is made
 *
 * If the declaration is disabled but inside a disabled function,
 * returns `true` but do not issue an error message.
 *
 * Params:
 *   d = Declaration to check
 *   loc = Location information of the call
 *   sc  = Scope in which the call occurs
 *   isAliasedDeclaration = if `true` searches overload set
 *
 * Returns:
 *   `true` if this `Declaration` is `@disable`d, `false` otherwise.
 */
bool checkDisabled(Declaration d, Loc loc, Scope* sc, bool isAliasedDeclaration = false)
{
    if (!(d.storage_class & STC.disable))
        return false;

    if (sc.func && sc.func.storage_class & STC.disable)
        return true;

    if (auto p = d.toParent())
    {
        if (auto postblit = d.isPostBlitDeclaration())
        {
            /* https://issues.dlang.org/show_bug.cgi?id=21885
             *
             * If the generated postblit is disabled, it
             * means that one of the fields has a disabled
             * postblit. Print the first field that has
             * a disabled postblit.
             */
            if (postblit.isGenerated())
            {
                auto sd = p.isStructDeclaration();
                assert(sd);
                for (size_t i = 0; i < sd.fields.length; i++)
                {
                    auto structField = sd.fields[i];
                    if (structField.overlapped)
                        continue;
                    Type tv = structField.type.baseElemOf();
                    if (tv.ty != Tstruct)
                        continue;
                    auto sdv = (cast(TypeStruct)tv).sym;
                    if (!sdv.postblit)
                        continue;
                    if (sdv.postblit.isDisabled())
                    {
                        .error(loc, "%s `%s` is not copyable because field `%s` is not copyable", p.kind, p.toPrettyChars, structField.toChars());
                        return true;
                    }
                }
            }
            .error(loc, "%s `%s` is not copyable because it has a disabled postblit", p.kind, p.toPrettyChars);
            return true;
        }
    }

    // if the function is @disabled, maybe there
    // is an overload in the overload set that isn't
    if (isAliasedDeclaration)
    {
        if (FuncDeclaration fd = d.isFuncDeclaration())
        {
            for (FuncDeclaration ovl = fd; ovl; ovl = cast(FuncDeclaration)ovl.overnext)
                if (!(ovl.storage_class & STC.disable))
                    return false;
        }
    }

    if (auto ctor = d.isCtorDeclaration())
    {
        //printf("checkDisabled() %s %s\n", ctor.toPrettyChars(), toChars(ctor.type));
        if (ctor.isCpCtor && ctor.isGenerated())
        {
            .error(loc, "generating an `inout` copy constructor for `struct %s` failed, therefore instances of it are uncopyable", d.parent.toPrettyChars());
            return true;
        }
    }
    .error(loc, "%s `%s` cannot be used because it is annotated with `@disable`", d.kind, d.toPrettyChars);
    return true;
}

/*******************************************
 * Helper function for the expansion of manifest constant.
 */
private Expression expandInitializer(VarDeclaration vd, Loc loc)
{
    assert((vd.storage_class & STC.manifest) && vd._init);

    auto e = vd.getConstInitializer();
    if (!e)
    {
        .error(loc, "cannot make expression out of initializer for `%s`", vd.toChars());
        return ErrorExp.get();
    }

    e = e.copy();
    e.loc = loc;    // for better error message
    return e;
}

/*****************************************************
 * Determine if template instance is really a template function,
 * and that template function needs to infer types from the function
 * arguments.
 *
 * Like findBestMatch, iterate possible template candidates,
 * but just looks only the necessity of type inference.
 */
private bool needsTypeInference(TemplateInstance ti, Scope* sc, int flag = 0)
{
    //printf("TemplateInstance.needsTypeInference() %s\n", toChars());
    if (ti.semanticRun != PASS.initial)
        return false;

    const olderrs = global.errors;
    Objects dedtypes;
    size_t count = 0;

    auto tovers = ti.tempdecl.isOverloadSet();
    foreach (size_t oi; 0 .. tovers ? tovers.a.length : 1)
    {
        Dsymbol dstart = tovers ? tovers.a[oi] : ti.tempdecl;
        int r = overloadApply(dstart, (Dsymbol s)
        {
            auto td = s.isTemplateDeclaration();
            if (!td)
                return 0;

            /* If any of the overloaded template declarations need inference,
             * then return true
             */
            if (!td.onemember)
                return 0;
            if (auto td2 = td.onemember.isTemplateDeclaration())
            {
                if (!td2.onemember || !td2.onemember.isFuncDeclaration())
                    return 0;
                if (ti.tiargs.length >= td.parameters.length - (td.isVariadic() ? 1 : 0))
                    return 0;
                return 1;
            }
            auto fd = td.onemember.isFuncDeclaration();
            if (!fd || fd.type.ty != Tfunction)
                return 0;

            foreach (tp; *td.parameters)
            {
                if (tp.isTemplateThisParameter())
                    return 1;
            }

            /* Determine if the instance arguments, tiargs, are all that is necessary
             * to instantiate the template.
             */
            //printf("tp = %p, td.parameters.length = %d, tiargs.length = %d\n", tp, td.parameters.length, tiargs.length);
            auto tf = fd.type.isTypeFunction();
            if (tf.parameterList.length)
            {
                auto tp = td.isVariadic();
                if (tp && td.parameters.length > 1)
                    return 1;

                if (!tp && ti.tiargs.length < td.parameters.length)
                {
                    // Can remain tiargs be filled by default arguments?
                    foreach (size_t i; ti.tiargs.length .. td.parameters.length)
                    {
                        if (!(*td.parameters)[i].hasDefaultArg())
                            return 1;
                    }
                }

                foreach (i, fparam; tf.parameterList)
                {
                    // 'auto ref' needs inference.
                    if (fparam.storageClass & STC.auto_)
                        return 1;
                }
            }

            if (!flag)
            {
                /* Calculate the need for overload resolution.
                 * When only one template can match with tiargs, inference is not necessary.
                 */
                dedtypes.setDim(td.parameters.length);
                dedtypes.zero();
                if (td.semanticRun == PASS.initial)
                {
                    if (td._scope)
                    {
                        // Try to fix forward reference. Ungag errors while doing so.
                        Ungag ungag = td.ungagSpeculative();
                        td.dsymbolSemantic(td._scope);
                    }
                    if (td.semanticRun == PASS.initial)
                    {
                        .error(ti.loc, "%s `%s` `%s` forward references template declaration `%s`",
                               ti.kind, ti.toPrettyChars, ti.toChars(), td.toChars());
                        return 1;
                    }
                }
                MATCH m = matchWithInstance(sc, td, ti, dedtypes, ArgumentList(), 0);
                if (m == MATCH.nomatch)
                    return 0;
            }

            /* If there is more than one function template which matches, we may
             * need type inference (see https://issues.dlang.org/show_bug.cgi?id=4430)
             */
            return ++count > 1 ? 1 : 0;
        });
        if (r)
            return true;
    }

    if (olderrs != global.errors)
    {
        if (!global.gag)
        {
            errorSupplemental(ti.loc, "while looking for match for `%s`", ti.toChars());
            ti.semanticRun = PASS.semanticdone;
            ti.inst = ti;
        }
        ti.errors = true;
    }
    //printf("false\n");
    return false;
}

/***************************************
 * Fill out remainder of elements[] with default initializers for fields[].
 * Params:
 *      sd          = struct
 *      loc         = location
 *      elements    = explicit arguments which given to construct object.
 *      ctorinit    = true if the elements will be used for default initialization.
 * Returns:
 *      false if any errors occur.
 *      Otherwise, returns true and the missing arguments will be pushed in elements[].
 */
bool fill(StructDeclaration sd, const ref Loc loc, ref Expressions elements, bool ctorinit)
{
    //printf("AggregateDeclaration::fill() %s\n", toChars());
    assert(sd.sizeok == Sizeok.done);
    const nfields = sd.nonHiddenFields();
    bool errors = false;

    size_t dim = elements.length;
    elements.setDim(nfields);
    foreach (size_t i; dim .. nfields)
        elements[i] = null;

    // Fill in missing any elements with default initializers
    foreach (i; 0 .. nfields)
    {
        if (elements[i])
            continue;

        auto vd = sd.fields[i];
        auto vx = vd;
        if (vd._init && vd._init.isVoidInitializer())
            vx = null;

        // Find overlapped fields with the hole [vd.offset .. vd.offset.size()].
        size_t fieldi = i;
        foreach (j; 0 .. nfields)
        {
            if (i == j)
                continue;
            auto v2 = sd.fields[j];
            if (!vd.isOverlappedWith(v2))
                continue;

            if (elements[j])
            {
                vx = null;
                break;
            }
            if (v2._init && v2._init.isVoidInitializer())
                continue;

            version (all)
            {
                /* Prefer first found non-void-initialized field
                 * union U { int a; int b = 2; }
                 * U u;    // Error: overlapping initialization for field a and b
                 */
                if (!vx)
                {
                    vx = v2;
                    fieldi = j;
                }
                else if (v2._init)
                {
                    .error(loc, "overlapping initialization for field `%s` and `%s`", v2.toChars(), vd.toChars());
                    errors = true;
                }
            }
            else
            {
                // fixes https://issues.dlang.org/show_bug.cgi?id=1432 by enabling this path always

                /* Prefer explicitly initialized field
                 * union U { int a; int b = 2; }
                 * U u;    // OK (u.b == 2)
                 */
                if (!vx || !vx._init && v2._init)
                {
                    vx = v2;
                    fieldi = j;
                }
                else if (vx != vd && !vx.isOverlappedWith(v2))
                {
                    // Both vx and v2 fills vd, but vx and v2 does not overlap
                }
                else if (vx._init && v2._init)
                {
                    .error(loc, "overlapping default initialization for field `%s` and `%s`",
                        v2.toChars(), vd.toChars());
                    errors = true;
                }
                else
                    assert(vx._init || !vx._init && !v2._init);
            }
        }
        if (!vx)
            continue;

        Expression e;
        if (vx.type.size() == 0)
        {
            e = null;
        }
        else if (vx._init)
        {
            assert(!vx._init.isVoidInitializer());
            if (vx.inuse)   // https://issues.dlang.org/show_bug.cgi?id=18057
            {
                .error(loc, "%s `%s` recursive initialization of field", vx.kind(), vx.toPrettyChars());
                errors = true;
            }
            else
                e = vx.getConstInitializer(false);
        }
        else
        {
            if ((vx.storage_class & STC.nodefaultctor) && !ctorinit)
            {
                .error(loc, "field `%s.%s` must be initialized because it has no default constructor",
                    sd.type.toChars(), vx.toChars());
                errors = true;
            }
            /* https://issues.dlang.org/show_bug.cgi?id=12509
             * Get the element of static array type.
             */
            Type telem = vx.type;
            if (telem.ty == Tsarray)
            {
                /* We cannot use Type::baseElemOf() here.
                 * If the bottom of the Tsarray is an enum type, baseElemOf()
                 * will return the base of the enum, and its default initializer
                 * would be different from the enum's.
                 */
                TypeSArray tsa;
                while ((tsa = telem.toBasetype().isTypeSArray()) !is null)
                    telem = tsa.next;
                if (telem.ty == Tvoid)
                    telem = Type.tuns8.addMod(telem.mod);
            }
            if (telem.needsNested() && ctorinit)
                e = telem.defaultInit(loc);
            else
                e = telem.defaultInitLiteral(loc);
        }
        elements[fieldi] = e;
    }
    foreach (e; elements)
    {
        if (e && e.op == EXP.error)
            return false;
    }

    return !errors;
}
