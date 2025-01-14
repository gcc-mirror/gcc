/**
 * Semantic analysis for cast-expressions.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dcast.d, _dcast.d)
 * Documentation:  https://dlang.org/phobos/dmd_dcast.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dcast.d
 */

module dmd.dcast;

import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.aliasthis;
import dmd.arrayop;
import dmd.arraytypes;
import dmd.astenums;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dinterpret;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.location;
import dmd.impcnvtab;
import dmd.importc;
import dmd.init;
import dmd.intrange;
import dmd.mtype;
import dmd.opover;
import dmd.optimize;
import dmd.root.ctfloat;
import dmd.common.outbuffer;
import dmd.root.rmem;
import dmd.root.utf;
import dmd.safe : setUnsafe;
import dmd.tokens;
import dmd.typesem;

enum LOG = false;

/**
 * Attempt to implicitly cast the expression into type `t`.
 *
 * This routine will change `e`. To check the matching level,
 * use `implicitConvTo`.
 *
 * Params:
 *   e = Expression that is to be casted
 *   sc = Current scope
 *   t = Expected resulting type
 *
 * Returns:
 *   The resulting casted expression (mutating `e`), or `ErrorExp`
 *    if such an implicit conversion is not possible.
 */
Expression implicitCastTo(Expression e, Scope* sc, Type t)
{
    Expression visit(Expression e)
    {
        //printf("Expression.implicitCastTo(%s of type %s) => %s\n", e.toChars(), e.type.toChars(), t.toChars());
        if (const match = (sc && sc.inCfile) ? e.cimplicitConvTo(t) : e.implicitConvTo(t))
        {
            // no need for an extra cast when matching is exact

            if (match == MATCH.convert && e.type.isTypeNoreturn() && e.op != EXP.type)
            {
                return specialNoreturnCast(e, t);
            }
            if (match == MATCH.constant && (e.type.constConv(t) || !e.isLvalue() && e.type.equivalent(t)))
            {
                /* Do not emit CastExp for const conversions and
                 * unique conversions on rvalue.
                 */
                auto result = e.copy();
                result.type = t;
                return result;
            }

            auto ad = isAggregate(e.type);
            if (ad && ad.aliasthis)
            {
                if (!ad.type || ad.type.isTypeError())
                    return e;
                auto ts = ad.type.isTypeStruct();
                const adMatch = ts
                    ? ts.implicitConvToWithoutAliasThis(t)
                    : ad.type.isTypeClass().implicitConvToWithoutAliasThis(t);

                if (!adMatch)
                {
                    Type tob = t.toBasetype();
                    Type t1b = e.type.toBasetype();
                    if (ad != isAggregate(tob))
                    {
                        if (t1b.ty == Tclass && tob.ty == Tclass)
                        {
                            ClassDeclaration t1cd = t1b.isClassHandle();
                            ClassDeclaration tocd = tob.isClassHandle();
                            int offset;
                            if (tocd.isBaseOf(t1cd, &offset))
                            {
                                auto result = new CastExp(e.loc, e, t);
                                result.type = t;
                                return result;
                            }
                        }

                        /* Forward the cast to our alias this member, rewrite to:
                         *   cast(to)e1.aliasthis
                         */
                        auto result = resolveAliasThis(sc, e);
                        return result.castTo(sc, t);
                   }
                }
            }

            return e.castTo(sc, t);
        }

        auto result = e.optimize(WANTvalue);
        if (result != e)
        {
            return implicitCastTo(result, sc, t);
        }

        if (t.ty != Terror && e.type.ty != Terror)
        {
            if (!t.deco)
            {
                error(e.loc, "forward reference to type `%s`", t.toChars());
            }
            else
            {
                //printf("type %p ty %d deco %p\n", type, type.ty, type.deco);
                //type = type.typeSemantic(loc, sc);
                //printf("type %s t %s\n", type.deco, t.deco);
                auto ts = toAutoQualChars(e.type, t);
                error(e.loc, "cannot implicitly convert expression `%s` of type `%s` to `%s`",
                    e.toChars(), ts[0], ts[1]);
            }
        }
        return ErrorExp.get();
    }

    Expression visitString(StringExp e)
    {
        //printf("StringExp::implicitCastTo(%s of type %s) => %s\n", e.toChars(), e.type.toChars(), t.toChars());
        auto result = visit(e);
        if (auto se = result.isStringExp())
        {
            // Retain polysemous nature if it started out that way
            se.committed = e.committed;
        }
        return result;
    }

    Expression visitError(ErrorExp e)
    {
        return e;
    }

    Expression visitFunc(FuncExp e)
    {
        //printf("FuncExp::implicitCastTo type = %p %s, t = %s\n", e.type, e.type ? e.type.toChars() : NULL, t.toChars());
        FuncExp fe;
        if (e.matchType(t, sc, &fe, global.errorSink) > MATCH.nomatch)
        {
            return fe;
        }
        return visit(e);
    }

    Expression visitArrayLiteral(ArrayLiteralExp e)
    {
        auto result = visit(e);

        Type tb = result.type.toBasetype();
        if (auto ta = tb.isTypeDArray())
            if (global.params.useTypeInfo && Type.dtypeinfo)
                semanticTypeInfo(sc, ta.next);
        return result;
    }

    Expression visitSlice(SliceExp e)
    {
        auto result = visit(e);

        if (auto se = result.isSliceExp())
            if (auto ale = se.e1.isArrayLiteralExp())
            {
                Type tb = t.toBasetype();
                Type tx = (tb.ty == Tsarray)
                    ? tb.nextOf().sarrayOf(ale.elements ? ale.elements.length : 0)
                    : tb.nextOf().arrayOf();
                se.e1 = ale.implicitCastTo(sc, tx);
            }

        return result;
    }

    switch (e.op)
    {
        default              : return visit            (e);
        case EXP.string_     : return visitString      (e.isStringExp());
        case EXP.error       : return visitError       (e.isErrorExp());
        case EXP.function_   : return visitFunc        (e.isFuncExp());
        case EXP.arrayLiteral: return visitArrayLiteral(e.isArrayLiteralExp());
        case EXP.slice       : return visitSlice       (e.isSliceExp());
    }
}

/**
 * Checks whether or not an expression can be implicitly converted
 * to type `t`.
 *
 * Unlike `implicitCastTo`, this routine does not perform the actual cast,
 * but only checks up to what `MATCH` level the conversion would be possible.
 *
 * Params:
 *   e = Expression that is to be casted
 *   t = Expected resulting type
 *
 * Returns:
 *   The `MATCH` level between `e.type` and `t`.
 */
MATCH implicitConvTo(Expression e, Type t)
{
    MATCH visit(Expression e)
    {
        version (none)
        {
            printf("Expression::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        //static int nest; if (++nest == 10) assert(0);
        if (t == Type.terror)
            return MATCH.nomatch;
        if (!e.type)
        {
            error(e.loc, "`%s` is not an expression", e.toChars());
            e.type = Type.terror;
        }

        Expression ex = e.optimize(WANTvalue);
        if (ex.type.equals(t))
        {
            return MATCH.exact;
        }
        if (ex != e)
        {
            //printf("\toptimized to %s of type %s\n", e.toChars(), e.type.toChars());
            return ex.implicitConvTo(t);
        }

        MATCH match = e.type.implicitConvTo(t);
        if (match != MATCH.nomatch)
        {
            return match;
        }

        /* See if we can do integral narrowing conversions
         */
        if (e.type.isIntegral() && t.isIntegral() && e.type.isTypeBasic() && t.isTypeBasic())
        {
            IntRange src = getIntRange(e);
            IntRange target = IntRange.fromType(t);
            if (target.contains(src))
            {
                return MATCH.convert;
            }
        }
        return MATCH.nomatch;
    }

    /******
     * Given expression e of type t, see if we can implicitly convert e
     * to type tprime, where tprime is type t with mod bits added.
     * Returns:
     *      match level
     */
    static MATCH implicitMod(Expression e, Type t, MOD mod)
    {
        Type tprime;
        if (t.ty == Tpointer)
            tprime = t.nextOf().castMod(mod).pointerTo();
        else if (t.ty == Tarray)
            tprime = t.nextOf().castMod(mod).arrayOf();
        else if (t.ty == Tsarray)
            tprime = t.nextOf().castMod(mod).sarrayOf(t.size() / t.nextOf().size());
        else
            tprime = t.castMod(mod);

        return e.implicitConvTo(tprime);
    }

    static MATCH implicitConvToAddMin(BinExp e, Type t)
    {
        /* Is this (ptr +- offset)? If so, then ask ptr
         * if the conversion can be done.
         * This is to support doing things like implicitly converting a mutable unique
         * pointer to an immutable pointer.
         */

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (typeb.ty != Tpointer || tb.ty != Tpointer)
            return MATCH.nomatch;

        Type t1b = e.e1.type.toBasetype();
        Type t2b = e.e2.type.toBasetype();
        if (t1b.ty == Tpointer && t2b.isIntegral() && t1b.equivalent(tb))
        {
            // ptr + offset
            // ptr - offset
            MATCH m = e.e1.implicitConvTo(t);
            return (m > MATCH.constant) ? MATCH.constant : m;
        }
        if (t2b.ty == Tpointer && t1b.isIntegral() && t2b.equivalent(tb))
        {
            // offset + ptr
            MATCH m = e.e2.implicitConvTo(t);
            return (m > MATCH.constant) ? MATCH.constant : m;
        }

        return MATCH.nomatch;
    }

    // Apply mod bits to each function parameter,
    // and see if we can convert the function argument to the modded type
    static bool parametersModMatch(Expressions* args, TypeFunction tf, MOD mod)
    {
        const size_t nparams = tf.parameterList.length;
        const size_t j = tf.isDstyleVariadic(); // if TypeInfoArray was prepended
        foreach (const i; j .. args.length)
        {
            Expression earg = (*args)[i];
            Type targ = earg.type.toBasetype();
            static if (LOG)
            {
                printf("[%d] earg: %s, targ: %s\n", cast(int)i, earg.toChars(), targ.toChars());
            }
            if (i - j < nparams)
            {
                Parameter fparam = tf.parameterList[i - j];
                if (fparam.isLazy())
                    return false; // not sure what to do with this
                Type tparam = fparam.type;
                if (!tparam)
                    continue;
                if (fparam.isReference())
                {
                    if (targ.constConv(tparam.castMod(mod)) == MATCH.nomatch)
                        return false;
                    continue;
                }
            }
            static if (LOG)
            {
                printf("[%d] earg: %s, targm: %s\n", cast(int)i, earg.toChars(), targ.addMod(mod).toChars());
            }
            if (implicitMod(earg, targ, mod) == MATCH.nomatch)
                return false;
        }
        return true;
    }

    MATCH visitAdd(AddExp e)
    {
        version (none)
        {
            printf("AddExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = visit(e);
        if (result == MATCH.nomatch)
            result = implicitConvToAddMin(e, t);
        return result;
    }

    MATCH visitMin(MinExp e)
    {
        version (none)
        {
            printf("MinExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = visit(e);
        if (result == MATCH.nomatch)
            result = implicitConvToAddMin(e, t);
        return result;
    }

    MATCH visitInteger(IntegerExp e)
    {
        version (none)
        {
            printf("IntegerExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        MATCH m = e.type.implicitConvTo(t);
        if (m >= MATCH.constant)
        {
            return m;
        }

        TY ty = e.type.toBasetype().ty;
        TY toty = t.toBasetype().ty;
        TY oldty = ty;

        if (m == MATCH.nomatch && t.ty == Tenum)
            return MATCH.nomatch;

        if (auto tv = t.isTypeVector())
        {
            TypeBasic tb = tv.elementType();
            if (tb.ty == Tvoid)
                return MATCH.nomatch;
            toty = tb.ty;
        }

        switch (ty)
        {
        case Tbool:
        case Tint8:
        case Tchar:
        case Tuns8:
        case Tint16:
        case Tuns16:
        case Twchar:
            ty = Tint32;
            break;

        case Tdchar:
            ty = Tuns32;
            break;

        default:
            break;
        }

        // Only allow conversion if no change in value
        immutable dinteger_t value = e.toInteger();

        bool isLosslesslyConvertibleToFP(T)()
        {
            if (e.type.isUnsigned())
            {
                const f = cast(T) value;
                return cast(dinteger_t) f == value;
            }

            const f = cast(T) cast(sinteger_t) value;
            return cast(sinteger_t) f == cast(sinteger_t) value;
        }

        switch (toty)
        {
        case Tbool:
            if ((value & 1) != value)
                return MATCH.nomatch;
            break;

        case Tint8:
            if (ty == Tuns64 && value & ~0x7FU)
                return MATCH.nomatch;
            if (cast(byte)value != value)
                return MATCH.nomatch;
            break;

        case Tchar:
            if ((oldty == Twchar || oldty == Tdchar) && value > 0x7F)
                return MATCH.nomatch;
            goto case Tuns8;
        case Tuns8:
            //printf("value = %llu %llu\n", cast(dinteger_t)cast(ubyte)value, value);
            if (cast(ubyte)value != value)
                return MATCH.nomatch;
            break;

        case Tint16:
            if (ty == Tuns64 && value & ~0x7FFFU)
                return MATCH.nomatch;
            if (cast(short)value != value)
                return MATCH.nomatch;
            break;

        case Twchar:
            if (oldty == Tdchar && value > 0xD7FF && value < 0xE000)
                return MATCH.nomatch;
            goto case Tuns16;
        case Tuns16:
            if (cast(ushort)value != value)
                return MATCH.nomatch;
            break;

        case Tint32:
            if (ty == Tuns32)
            {
            }
            else if (ty == Tuns64 && value & ~0x7FFFFFFFU)
                return MATCH.nomatch;
            else if (cast(int)value != value)
                return MATCH.nomatch;
            break;

        case Tuns32:
            if (ty == Tint32)
            {
            }
            else if (cast(uint)value != value)
                return MATCH.nomatch;
            break;

        case Tdchar:
            if (value > 0x10FFFFU)
                return MATCH.nomatch;
            break;

        case Tfloat32:
            if (!isLosslesslyConvertibleToFP!float)
                return MATCH.nomatch;
            break;

        case Tfloat64:
            if (!isLosslesslyConvertibleToFP!double)
                return MATCH.nomatch;
            break;

        case Tfloat80:
            if (!isLosslesslyConvertibleToFP!real_t)
                return MATCH.nomatch;
            break;

        case Tpointer:
            //printf("type = %s\n", type.toBasetype().toChars());
            //printf("t = %s\n", t.toBasetype().toChars());
            if (ty == Tpointer && e.type.toBasetype().nextOf().ty == t.toBasetype().nextOf().ty)
            {
                /* Allow things like:
                 *      const char* P = cast(char *)3;
                 *      char* q = P;
                 */
                break;
            }
            goto default;

        default:
            return visit(e);
        }

        //printf("MATCH.convert\n");
        return MATCH.convert;
    }

    MATCH visitError(ErrorExp e)
    {
        return MATCH.nomatch;
    }

    MATCH visitNull(NullExp e)
    {
        version (none)
        {
            printf("NullExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        if (e.type.equals(t))
        {
            return MATCH.exact;
        }

        /* Allow implicit conversions from immutable to mutable|const,
         * and mutable to immutable. It works because, after all, a null
         * doesn't actually point to anything.
         */
        if (t.equivalent(e.type))
        {
            return MATCH.constant;
        }

        return visit(e);
    }

    MATCH visitStructLiteral(StructLiteralExp e)
    {
        version (none)
        {
            printf("StructLiteralExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;
        if (e.type.ty == t.ty && e.type.isTypeStruct() && e.type.isTypeStruct().sym == t.isTypeStruct().sym)
        {
            result = MATCH.constant;
            foreach (i, el; (*e.elements)[])
            {
                if (!el)
                    continue;
                Type te = e.sd.fields[i].type.addMod(t.mod);
                MATCH m2 = el.implicitConvTo(te);
                //printf("\t%s => %s, match = %d\n", el.toChars(), te.toChars(), m2);
                if (m2 < result)
                    result = m2;
            }
        }
        return result;
    }

    MATCH visitString(StringExp e)
    {
        version (none)
        {
            printf("StringExp::implicitConvTo(this=%s, committed=%d, type=%s, t=%s)\n", e.toChars(), e.committed, e.type.toChars(), t.toChars());
        }
        if (!e.committed && t.ty == Tpointer && t.nextOf().ty == Tvoid)
            return MATCH.nomatch;

        if (!(e.type.ty == Tsarray || e.type.ty == Tarray || e.type.ty == Tpointer))
            return visit(e);

        TY tyn = e.type.nextOf().ty;

        if (!tyn.isSomeChar && !e.hexString)
            return visit(e);

        switch (t.ty)
        {
        case Tsarray:
            if (e.type.ty == Tsarray)
            {
                TY tynto = t.nextOf().ty;
                if (tynto == tyn)
                {
                    if (e.type.isTypeSArray().dim.toInteger() == t.isTypeSArray().dim.toInteger())
                    {
                        return MATCH.exact;
                    }
                    return MATCH.nomatch;
                }
                if (tynto.isSomeChar)
                {
                    if (e.committed && tynto != tyn)
                        return MATCH.nomatch;
                    size_t fromlen = e.numberOfCodeUnits(tynto);
                    size_t tolen = cast(size_t)t.isTypeSArray().dim.toInteger();
                    if (tolen < fromlen)
                        return MATCH.nomatch;
                    if (tolen != fromlen)
                    {
                        // implicit length extending
                        return MATCH.convert;
                    }
                }
                if (!e.committed && tynto.isSomeChar)
                {
                    return MATCH.exact;
                }
            }
            else if (e.type.ty == Tarray)
            {
                TY tynto = t.nextOf().ty;
                if (tynto.isSomeChar)
                {
                    if (e.committed && tynto != tyn)
                        return MATCH.nomatch;
                    size_t fromlen = e.numberOfCodeUnits(tynto);
                    size_t tolen = cast(size_t)t.isTypeSArray().dim.toInteger();
                    if (tolen < fromlen)
                        return MATCH.nomatch;
                    if (tolen != fromlen)
                    {
                        // implicit length extending
                        return MATCH.convert;
                    }
                }
                if (tynto == tyn)
                {
                    return MATCH.exact;
                }
                if (!e.committed && tynto.isSomeChar)
                {
                    return MATCH.exact;
                }
            }
            goto case; /+ fall through +/
        case Tarray:
        case Tpointer:
            Type tn = t.nextOf();
            MATCH m = MATCH.exact;
            if (e.type.nextOf().mod != tn.mod)
            {
                // https://issues.dlang.org/show_bug.cgi?id=16183
                if (!tn.isConst() && !tn.isImmutable())
                    return MATCH.nomatch;
                m = MATCH.constant;
            }
            if (e.type != t && e.hexString && tn.isIntegral && (tn.size == e.sz || (!e.committed && (e.len % tn.size) == 0)))
            {
                m = MATCH.convert;
                return m;
            }
            if (!e.committed)
            {
                switch (tn.ty)
                {
                case Tchar:
                    if (e.postfix == 'w' || e.postfix == 'd')
                        m = MATCH.convert;
                    return m;
                case Twchar:
                    if (e.postfix != 'w')
                        m = MATCH.convert;
                    return m;
                case Tdchar:
                    if (e.postfix != 'd')
                        m = MATCH.convert;
                    return m;
                case Tenum:
                    if (tn.isTypeEnum().sym.isSpecial())
                    {
                        /* Allow string literal -> const(wchar_t)[]
                         */
                        if (TypeBasic tob = tn.toBasetype().isTypeBasic())
                        return tn.implicitConvTo(tob);
                    }
                    break;
                default:
                    break;
                }
            }
            break;

        default:
            break;
        }

        return visit(e);
    }

    MATCH visitArrayLiteral(ArrayLiteralExp e)
    {
        version (none)
        {
            printf("ArrayLiteralExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        auto result = MATCH.nomatch;
        if ((tb.ty == Tarray || tb.ty == Tsarray) &&
            (typeb.ty == Tarray || typeb.ty == Tsarray))
        {
            result = MATCH.exact;
            Type typen = typeb.nextOf().toBasetype();

            if (auto tsa = tb.isTypeSArray())
            {
                if (e.elements.length != tsa.dim.toInteger())
                    result = MATCH.nomatch;
            }

            Type telement = tb.nextOf();
            if (!e.elements.length)
            {
                if (typen.ty != Tvoid)
                    result = typen.implicitConvTo(telement);
            }
            else
            {
                if (e.basis)
                {
                    MATCH m = e.basis.implicitConvTo(telement);
                    if (m < result)
                        result = m;
                }
                for (size_t i = 0; i < e.elements.length; i++)
                {
                    Expression el = (*e.elements)[i];
                    if (result == MATCH.nomatch)
                        break;
                    if (!el)
                        continue;
                    MATCH m = el.implicitConvTo(telement);
                    if (m < result)
                        result = m; // remember worst match
                }
            }

            if (!result)
                result = e.type.implicitConvTo(t);

            return result;
        }
        else if (tb.ty == Tvector && (typeb.ty == Tarray || typeb.ty == Tsarray || typeb.ty == Tpointer))
        {   // Tpointer because ImportC eagerly converts Tsarray to Tpointer
            result = MATCH.exact;
            // Convert array literal to vector type
            TypeVector tv = tb.isTypeVector();
            TypeSArray tbase = tv.basetype.isTypeSArray();
            assert(tbase);
            const edim = e.elements.length;
            const tbasedim = tbase.dim.toInteger();
            if (edim > tbasedim)
            {
                return MATCH.nomatch;
            }

            Type telement = tv.elementType();
            if (edim < tbasedim)
            {
                Expression el = typeb.nextOf.defaultInitLiteral(e.loc);
                MATCH m = el.implicitConvTo(telement);
                if (m < result)
                    result = m; // remember worst match
            }
            foreach (el; (*e.elements)[])
            {
                MATCH m = el.implicitConvTo(telement);
                if (m < result)
                    result = m; // remember worst match
                if (result == MATCH.nomatch)
                    break; // no need to check for worse
            }
            return result;
        }

        return visit(e);
    }

    MATCH visitAssocArrayLiteral(AssocArrayLiteralExp e)
    {
        auto taa = t.toBasetype().isTypeAArray();
        Type typeb = e.type.toBasetype();

        if (!(taa && typeb.ty == Taarray))
            return visit(e);

        auto result = MATCH.exact;
        foreach (i, el; (*e.keys)[])
        {
            MATCH m = el.implicitConvTo(taa.index);
            if (m < result)
                result = m; // remember worst match
            if (result == MATCH.nomatch)
                break; // no need to check for worse
            el = (*e.values)[i];
            m = el.implicitConvTo(taa.nextOf());
            if (m < result)
                result = m; // remember worst match
            if (result == MATCH.nomatch)
                break; // no need to check for worse
        }
        return result;
    }

    MATCH visitCall(CallExp e)
    {
        enum LOG = false;
        static if (LOG)
        {
            printf("CallExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }

        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;

        /* Allow the result of strongly pure functions to
         * convert to immutable
         */
        if (e.f &&
            (!global.params.fixImmutableConv || e.f.isPure() >= PURE.const_) &&
            e.f.isReturnIsolated() // check isReturnIsolated last, because it is potentially expensive.
           )
        {
            result = e.type.immutableOf().implicitConvTo(t);
            if (result > MATCH.constant) // Match level is MATCH.constant at best.
                result = MATCH.constant;
            return result;
        }

        /* Conversion is 'const' conversion if:
         * 1. function is pure (weakly pure is ok)
         * 2. implicit conversion only fails because of mod bits
         * 3. each function parameter can be implicitly converted to the mod bits
         */
        auto tf = (e.f ? e.f.type : e.e1.type).toBasetype().isTypeFunction();
        if (!tf)
            return result;

        if (tf.purity == PURE.impure)
            return result;
        if (e.f && e.f.isNested())
            return result;

        /* See if fail only because of mod bits.
         *
         * https://issues.dlang.org/show_bug.cgi?id=14155
         * All pure functions can access global immutable data.
         * So the returned pointer may refer an immutable global data,
         * and then the returned pointer that points non-mutable object
         * cannot be unique pointer.
         *
         * Example:
         *  immutable g;
         *  static this() { g = 1; }
         *  const(int*) foo() pure { return &g; }
         *  void test() {
         *    immutable(int*) ip = foo(); // OK
         *    int* mp = foo();            // should be disallowed
         *  }
         */
        if (e.type.immutableOf().implicitConvTo(t) < MATCH.constant && e.type.addMod(MODFlags.shared_).implicitConvTo(t) < MATCH.constant && e.type.implicitConvTo(t.addMod(MODFlags.shared_)) < MATCH.constant)
        {
            return result;
        }
        // Allow a conversion to immutable type, or
        // conversions of mutable types between thread-local and shared.

        /* Get mod bits of what we're converting to
         */
        Type tb = t.toBasetype();
        MOD mod = tb.mod;
        if (tf.isRef)
        {
        }
        else
        {
            if (Type ti = getIndirection(t))
                mod = ti.mod;
        }
        static if (LOG)
        {
            printf("mod = x%x\n", mod);
        }
        if (mod & MODFlags.wild)
            return result; // not sure what to do with this

        /* Apply mod bits to each function parameter,
         * and see if we can convert the function argument to the modded type
         */
        if (auto dve = e.e1.isDotVarExp())
        {
            /* Treat 'this' as just another function argument
             */
            Type targ = dve.e1.type;
            if (targ.constConv(targ.castMod(mod)) == MATCH.nomatch)
                return result;
        }

        if (!parametersModMatch(e.arguments, tf, mod))
            return result;

        /* Success
         */
        return MATCH.constant;
    }

    MATCH visitAddr(AddrExp e)
    {
        version (none)
        {
            printf("AddrExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = e.type.implicitConvTo(t);
        //printf("\tresult = %d\n", result);

        if (result != MATCH.nomatch)
            return result;

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        // Look for pointers to functions where the functions are overloaded.
        if (e.e1.op == EXP.overloadSet &&
            (tb.ty == Tpointer || tb.ty == Tdelegate) && tb.nextOf().ty == Tfunction)
        {
            OverExp eo = e.e1.isOverExp();
            FuncDeclaration f = null;
            foreach (s; eo.vars.a[])
            {
                FuncDeclaration f2 = s.isFuncDeclaration();
                assert(f2);
                if (f2.overloadExactMatch(tb.nextOf()))
                {
                    if (f)
                    {
                        /* Error if match in more than one overload set,
                         * even if one is a 'better' match than the other.
                         */
                        ScopeDsymbol.multiplyDefined(e.loc, f, f2);
                    }
                    else
                        f = f2;
                    result = MATCH.exact;
                }
            }
        }

        if (e.e1.op == EXP.variable &&
            typeb.ty == Tpointer && typeb.nextOf().ty == Tfunction &&
            tb.ty == Tpointer && tb.nextOf().ty == Tfunction)
        {
            /* I don't think this can ever happen -
             * it should have been
             * converted to a SymOffExp.
             */
            assert(0);
        }

        //printf("\tresult = %d\n", result);
        return result;
    }

    MATCH visitSymOff(SymOffExp e)
    {
        version (none)
        {
            printf("SymOffExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = e.type.implicitConvTo(t);
        //printf("\tresult = %d\n", result);
        if (result != MATCH.nomatch)
            return result;

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        // Look for pointers to functions where the functions are overloaded.
        if (typeb.ty == Tpointer && typeb.nextOf().ty == Tfunction &&
            (tb.ty == Tpointer || tb.ty == Tdelegate) && tb.nextOf().ty == Tfunction)
        {
            if (FuncDeclaration f = e.var.isFuncDeclaration())
            {
                f = f.overloadExactMatch(tb.nextOf());
                if (f)
                {
                    if ((tb.ty == Tdelegate && (f.needThis() || f.isNested())) ||
                        (tb.ty == Tpointer && !(f.needThis() || f.isNested())))
                    {
                        result = MATCH.exact;
                    }
                }
            }
        }
        //printf("\tresult = %d\n", result);
        return result;
    }

    MATCH visitDelegate(DelegateExp e)
    {
        version (none)
        {
            printf("DelegateExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = e.type.implicitConvTo(t);
        if (result != MATCH.nomatch)
            return result;

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        // Look for pointers to functions where the functions are overloaded.
        if (typeb.ty == Tdelegate && tb.ty == Tdelegate)
        {
            if (e.func && e.func.overloadExactMatch(tb.nextOf()))
                result = MATCH.exact;
        }
        return result;
    }

    MATCH visitFunc(FuncExp e)
    {
        //printf("FuncExp::implicitConvTo type = %p %s, t = %s\n", e.type, e.type ? e.type.toChars() : NULL, t.toChars());
        MATCH m = e.matchType(t, null, null, global.errorSinkNull);
        if (m > MATCH.nomatch)
        {
            return m;
        }
        return visit(e);
    }

    MATCH visitAnd(AndExp e)
    {
        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;

        MATCH m1 = e.e1.implicitConvTo(t);
        MATCH m2 = e.e2.implicitConvTo(t);

        // Pick the worst match
        return (m1 < m2) ? m1 : m2;
    }

    MATCH visitOr(OrExp e)
    {
        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;

        MATCH m1 = e.e1.implicitConvTo(t);
        MATCH m2 = e.e2.implicitConvTo(t);

        // Pick the worst match
        return (m1 < m2) ? m1 : m2;
    }

    MATCH visitXor(XorExp e)
    {
        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;

        MATCH m1 = e.e1.implicitConvTo(t);
        MATCH m2 = e.e2.implicitConvTo(t);

        // Pick the worst match
        return (m1 < m2) ? m1 : m2;
    }

    MATCH visitCond(CondExp e)
    {
        e.econd = e.econd.optimize(WANTvalue);
        const opt = e.econd.toBool();
        if (opt.isPresent())
        {
            auto result = visit(e);
            if (result != MATCH.nomatch)
                return result;
        }

        MATCH m1 = e.e1.implicitConvTo(t);
        MATCH m2 = e.e2.implicitConvTo(t);
        //printf("CondExp: m1 %d m2 %d\n", m1, m2);

        // Pick the worst match
        return (m1 < m2) ? m1 : m2;
    }

    MATCH visitComma(CommaExp e)
    {
        return e.e2.implicitConvTo(t);
    }

    MATCH visitCast(CastExp e)
    {
        version (none)
        {
            printf("CastExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = e.type.implicitConvTo(t);
        if (result != MATCH.nomatch)
            return result;

        if (t.isIntegral() && e.e1.type.isIntegral() && e.e1.implicitConvTo(t) != MATCH.nomatch)
            result = MATCH.convert;
        else
            result = visit(e);
        return result;
    }

    MATCH visitNew(NewExp e)
    {
        version (none)
        {
            printf("NewExp::implicitConvTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;

        /* Calling new() is like calling a pure function. We can implicitly convert the
         * return from new() to t using the same algorithm as in CallExp, with the function
         * 'arguments' being:
         *    thisexp
         *    arguments
         *    .init
         * 'member' need to be pure.
         */

        /* See if fail only because of mod bits
         */
        if (e.type.immutableOf().implicitConvTo(t.immutableOf()) == MATCH.nomatch)
            return MATCH.nomatch;

        /* Get mod bits of what we're converting to
         */
        Type tb = t.toBasetype();
        MOD mod = tb.mod;
        if (Type ti = getIndirection(t))
            mod = ti.mod;
        static if (LOG)
        {
            printf("mod = x%x\n", mod);
        }
        if (mod & MODFlags.wild)
            return MATCH.nomatch; // not sure what to do with this

        /* Apply mod bits to each argument,
         * and see if we can convert the argument to the modded type
         */

        if (e.thisexp)
        {
            /* Treat 'this' as just another function argument
             */
            Type targ = e.thisexp.type;
            if (targ.constConv(targ.castMod(mod)) == MATCH.nomatch)
                return MATCH.nomatch;
        }

        /* Check call to 'member'
         */
        if (e.member)
        {
            FuncDeclaration fd = e.member;
            if (fd.errors || fd.type.ty != Tfunction)
                return MATCH.nomatch; // error
            TypeFunction tf = fd.type.isTypeFunction();
            if (tf.purity == PURE.impure)
                return MATCH.nomatch; // impure

            // Allow a conversion to immutable type, or
            // conversions of mutable types between thread-local and shared.
            if (e.type.immutableOf().implicitConvTo(t) < MATCH.constant && e.type.addMod(MODFlags.shared_).implicitConvTo(t) < MATCH.constant && e.type.implicitConvTo(t.addMod(MODFlags.shared_)) < MATCH.constant)
            {
                return MATCH.nomatch;
            }

            if (!parametersModMatch(e.arguments, tf, mod))
            {
                return MATCH.nomatch;
            }
        }

        /* If no 'member', then construction is by simple assignment,
         * and just straight check 'arguments'
         */
        if (!e.member && e.arguments)
        {
            for (size_t i = 0; i < e.arguments.length; ++i)
            {
                Expression earg = (*e.arguments)[i];
                if (!earg) // https://issues.dlang.org/show_bug.cgi?id=14853
                           // if it's on overlapped field
                    continue;
                Type targ = earg.type.toBasetype();
                static if (LOG)
                {
                    printf("[%d] earg: %s, targ: %s\n", cast(int)i, earg.toChars(), targ.toChars());
                    printf("[%d] earg: %s, targm: %s\n", cast(int)i, earg.toChars(), targ.addMod(mod).toChars());
                }
                if (implicitMod(earg, targ, mod) == MATCH.nomatch)
                    return MATCH.nomatch;
            }
        }

        /* Consider the .init expression as an argument
         */
        Type ntb = e.newtype.toBasetype();
        if (ntb.ty == Tarray)
            ntb = ntb.nextOf().toBasetype();
        if (auto ts = ntb.isTypeStruct())
        {
            // Don't allow nested structs - uplevel reference may not be convertible
            StructDeclaration sd = ts.sym;
            sd.size(e.loc); // resolve any forward references
            if (sd.isNested())
                return MATCH.nomatch;
        }
        if (ntb.isZeroInit(e.loc))
        {
            /* Zeros are implicitly convertible, except for special cases.
             */
            if (auto tc = ntb.isTypeClass())
            {
                /* With new() must look at the class instance initializer.
                 */
                ClassDeclaration cd = tc.sym;

                cd.size(e.loc); // resolve any forward references

                if (cd.isNested())
                    return MATCH.nomatch; // uplevel reference may not be convertible

                assert(!cd.isInterfaceDeclaration());

                struct ClassCheck
                {
                    extern (C++) static bool convertible(Expression e, ClassDeclaration cd, MOD mod)
                    {
                        for (size_t i = 0; i < cd.fields.length; i++)
                        {
                            VarDeclaration v = cd.fields[i];
                            Initializer _init = v._init;
                            if (_init)
                            {
                                if (_init.isVoidInitializer())
                                {
                                }
                                else if (ExpInitializer ei = _init.isExpInitializer())
                                {
                                    // https://issues.dlang.org/show_bug.cgi?id=21319
                                    // This is to prevent re-analyzing the same expression
                                    // over and over again.
                                    if (ei.exp == e)
                                        return false;
                                    Type tb = v.type.toBasetype();
                                    if (implicitMod(ei.exp, tb, mod) == MATCH.nomatch)
                                        return false;
                                }
                                else
                                {
                                    /* Enhancement: handle StructInitializer and ArrayInitializer
                                     */
                                    return false;
                                }
                            }
                            else if (!v.type.isZeroInit(e.loc))
                                return false;
                        }
                        return cd.baseClass ? convertible(e, cd.baseClass, mod) : true;
                    }
                }

                if (!ClassCheck.convertible(e, cd, mod))
                    return MATCH.nomatch;
            }
        }
        else
        {
            Expression earg = e.newtype.defaultInitLiteral(e.loc);
            Type targ = e.newtype.toBasetype();

            if (implicitMod(earg, targ, mod) == MATCH.nomatch)
                return MATCH.nomatch;
        }

        /* Success
         */
        return MATCH.constant;
    }

    MATCH visitSlice(SliceExp e)
    {
        //printf("SliceExp::implicitConvTo e = %s, type = %s\n", e.toChars(), e.type.toChars());
        auto result = visit(e);
        if (result != MATCH.nomatch)
            return result;

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (tb.ty == Tsarray && typeb.ty == Tarray)
        {
            typeb = toStaticArrayType(e);
            if (typeb)
            {
                // Try: T[] -> T[dim]
                // (Slice with compile-time known boundaries to static array)
                result = typeb.implicitConvTo(t);
                if (result > MATCH.convert)
                    result = MATCH.convert; // match with implicit conversion at most
            }
            return result;
        }

        /* If the only reason it won't convert is because of the mod bits,
         * then test for conversion by seeing if e1 can be converted with those
         * same mod bits.
         */
        Type t1b = e.e1.type.toBasetype();
        if (tb.ty == Tarray && typeb.equivalent(tb))
        {
            Type tbn = tb.nextOf();
            Type tx = null;

            /* If e.e1 is dynamic array or pointer, the uniqueness of e.e1
             * is equivalent with the uniqueness of the referred data. And in here
             * we can have arbitrary typed reference for that.
             */
            if (t1b.ty == Tarray)
                tx = tbn.arrayOf();
            if (t1b.ty == Tpointer)
                tx = tbn.pointerTo();

            /* If e.e1 is static array, at least it should be an rvalue.
             * If not, e.e1 is a reference, and its uniqueness does not link
             * to the uniqueness of the referred data.
             */
            if (t1b.ty == Tsarray && !e.e1.isLvalue())
                tx = tbn.sarrayOf(t1b.size() / tbn.size());

            if (tx)
            {
                result = e.e1.implicitConvTo(tx);
                if (result > MATCH.constant) // Match level is MATCH.constant at best.
                    result = MATCH.constant;
            }
        }

        // Enhancement 10724
        if (tb.ty == Tpointer && e.e1.op == EXP.string_)
            result = e.e1.implicitConvTo(t);
        return result;
    }

    MATCH visitTuple(TupleExp e)
    {
        auto result = e.type.implicitConvTo(t);
        if (result != MATCH.nomatch)
            return result;

        /* If target type is a tuple of same length, test conversion of
         * each expression to the corresponding type in the tuple.
         */
        TypeTuple totuple = t.isTypeTuple();
        if (totuple && e.exps.length == totuple.arguments.length)
        {
            result = MATCH.exact;
            foreach (i, ex; *e.exps)
            {
                auto to = (*totuple.arguments)[i].type;
                MATCH mi = ex.implicitConvTo(to);
                if (mi < result)
                    result = mi;
            }
        }
        return result;
    }

    switch (e.op)
    {
        default                   : return visit(e);
        case EXP.add              : return visitAdd(e.isAddExp());
        case EXP.min              : return visitMin(e.isMinExp());
        case EXP.int64            : return visitInteger(e.isIntegerExp());
        case EXP.error            : return visitError(e.isErrorExp());
        case EXP.null_            : return visitNull(e.isNullExp());
        case EXP.structLiteral    : return visitStructLiteral(e.isStructLiteralExp());
        case EXP.string_          : return visitString(e.isStringExp());
        case EXP.arrayLiteral     : return visitArrayLiteral(e.isArrayLiteralExp());
        case EXP.assocArrayLiteral: return visitAssocArrayLiteral(e.isAssocArrayLiteralExp());
        case EXP.call             : return visitCall(e.isCallExp());
        case EXP.address          : return visitAddr(e.isAddrExp());
        case EXP.symbolOffset     : return visitSymOff(e.isSymOffExp());
        case EXP.delegate_        : return visitDelegate(e.isDelegateExp());
        case EXP.function_        : return visitFunc(e.isFuncExp());
        case EXP.and              : return visitAnd(e.isAndExp());
        case EXP.or               : return visitOr(e.isOrExp());
        case EXP.xor              : return visitXor(e.isXorExp());
        case EXP.question         : return visitCond(e.isCondExp());
        case EXP.comma            : return visitComma(e.isCommaExp());
        case EXP.cast_            : return visitCast(e.isCastExp());
        case EXP.new_             : return visitNew(e.isNewExp());
        case EXP.slice            : return visitSlice(e.isSliceExp());
        case EXP.tuple            : return visitTuple(e.isTupleExp());
    }
}

/********************************
 * Determine if 'from' can be implicitly converted
 * to type 'to'.
 * Returns:
 *      MATCH.nomatch, MATCH.convert, MATCH.constant, MATCH.exact
 */
MATCH implicitConvTo(Type from, Type to)
{
    MATCH visitType(Type from)
    {
        //printf("Type::implicitConvTo(this=%p, to=%p)\n", this, to);
        //printf("from: %s\n", from.toChars());
        //printf("to  : %s\n", to.toChars());
        if (from.equals(to))
            return MATCH.exact;
        return MATCH.nomatch;

    }

    MATCH visitBasic(TypeBasic from)
    {
        //printf("TypeBasic::implicitConvTo(%s) from %s\n", to.toChars(), from.toChars());
        if (from == to)
            return MATCH.exact;

        if (from.ty == to.ty)
        {
            if (from.mod == to.mod)
                return MATCH.exact;
            if (MODimplicitConv(from.mod, to.mod))
                return MATCH.constant;
            if (!((from.mod ^ to.mod) & MODFlags.shared_)) // for wild matching
                return MATCH.constant;
            return MATCH.convert;
        }

        if (from.ty == Tvoid || to.ty == Tvoid)
            return MATCH.nomatch;
        if (to.ty == Tbool)
            return MATCH.nomatch;

        TypeBasic tob;
        if (to.ty == Tvector && to.deco)
        {
            TypeVector tv = cast(TypeVector)to;
            tob = tv.elementType();
        }
        else if (auto te = to.isTypeEnum())
        {
            EnumDeclaration ed = te.sym;
            if (ed.isSpecial())
            {
                /* Special enums that allow implicit conversions to them
                 * with a MATCH.convert
                 */
                tob = to.toBasetype().isTypeBasic();
            }
            else
                return MATCH.nomatch;
        }
        else
            tob = to.isTypeBasic();
        if (!tob)
            return MATCH.nomatch;

        if (from.flags & TFlags.integral)
        {
            // Disallow implicit conversion of integers to imaginary or complex
            if (tob.flags & (TFlags.imaginary | TFlags.complex))
                return MATCH.nomatch;

            // If converting from integral to integral
            if (tob.flags & TFlags.integral)
            {
                const sz = size(from, Loc.initial);
                const tosz = tob.size(Loc.initial);

                /* Can't convert to smaller size
                 */
                if (sz > tosz)
                    return MATCH.nomatch;
                /* Can't change sign if same size
                 */
                //if (sz == tosz && (flags ^ tob.flags) & TFlags.unsigned)
                //    return MATCH.nomatch;
            }
        }
        else if (from.flags & TFlags.floating)
        {
            // Disallow implicit conversion of floating point to integer
            if (tob.flags & TFlags.integral)
                return MATCH.nomatch;

            assert(tob.flags & TFlags.floating || to.ty == Tvector);

            // Disallow implicit conversion from complex to non-complex
            if (from.flags & TFlags.complex && !(tob.flags & TFlags.complex))
                return MATCH.nomatch;

            // Disallow implicit conversion of real or imaginary to complex
            if (from.flags & (TFlags.real_ | TFlags.imaginary) && tob.flags & TFlags.complex)
                return MATCH.nomatch;

            // Disallow implicit conversion to-from real and imaginary
            if ((from.flags & (TFlags.real_ | TFlags.imaginary)) != (tob.flags & (TFlags.real_ | TFlags.imaginary)))
                return MATCH.nomatch;
        }
        return MATCH.convert;

    }

    MATCH visitVector(TypeVector from)
    {
        //printf("TypeVector::implicitConvTo(%s) from %s\n", to.toChars(), from.toChars());
        if (from == to)
            return MATCH.exact;
        if (to.ty != Tvector)
            return MATCH.nomatch;

        TypeVector tv = cast(TypeVector)to;
        assert(from.basetype.ty == Tsarray && tv.basetype.ty == Tsarray);

        // Can't convert to a vector which has different size.
        if (from.basetype.size() != tv.basetype.size())
            return MATCH.nomatch;

        // Allow conversion to void[]
        if (tv.basetype.nextOf().ty == Tvoid)
            return MATCH.convert;

        // Otherwise implicitly convertible only if basetypes are.
        return from.basetype.implicitConvTo(tv.basetype);
    }

    MATCH visitSArray(TypeSArray from)
    {
        //printf("TypeSArray::implicitConvTo(to = %s) this = %s\n", to.toChars(), from.toChars());
        if (auto ta = to.isTypeDArray())
        {
            if (!MODimplicitConv(from.next.mod, ta.next.mod))
                return MATCH.nomatch;

            /* Allow conversion to void[]
             */
            if (ta.next.ty == Tvoid)
            {
                return MATCH.convert;
            }

            MATCH m = from.next.constConv(ta.next);
            if (m > MATCH.nomatch)
            {
                return MATCH.convert;
            }
            return MATCH.nomatch;
        }
        if (auto tsa = to.isTypeSArray())
        {
            if (from == to)
                return MATCH.exact;

            if (from.dim.equals(tsa.dim))
            {
                MATCH m = from.next.implicitConvTo(tsa.next);

                /* Allow conversion to non-interface base class.
                 */
                if (m == MATCH.convert &&
                    from.next.ty == Tclass)
                {
                    if (auto toc = tsa.next.isTypeClass)
                    {
                        if (!toc.sym.isInterfaceDeclaration)
                            return MATCH.convert;
                    }
                }

                /* Since static arrays are value types, allow
                 * conversions from const elements to non-const
                 * ones, just like we allow conversion from const int
                 * to int.
                 */
                if (m >= MATCH.constant)
                {
                    if (from.mod != to.mod)
                        m = MATCH.constant;
                    return m;
                }
            }
        }
        return MATCH.nomatch;
    }

    MATCH visitDArray(TypeDArray from)
    {
        //printf("TypeDArray::implicitConvTo(to = %s) this = %s\n", to.toChars(), from.toChars());
        if (from.equals(to))
            return MATCH.exact;

        if (auto ta = to.isTypeDArray())
        {
            if (!MODimplicitConv(from.next.mod, ta.next.mod))
                return MATCH.nomatch; // not const-compatible

            /* Allow conversion to void[]
             */
            if (from.next.ty != Tvoid && ta.next.ty == Tvoid)
            {
                return MATCH.convert;
            }

            MATCH m = from.next.constConv(ta.next);
            if (m > MATCH.nomatch)
            {
                if (m == MATCH.exact && from.mod != to.mod)
                    m = MATCH.constant;
                return m;
            }
        }

        return visitType(from);
    }

    MATCH visitAArray(TypeAArray from)
    {
        //printf("TypeAArray::implicitConvTo(to = %s) this = %s\n", to.toChars(), from.toChars());
        if (from.equals(to))
            return MATCH.exact;

        if (auto ta = to.isTypeAArray())
        {
            if (!MODimplicitConv(from.next.mod, ta.next.mod))
                return MATCH.nomatch; // not const-compatible

            if (!MODimplicitConv(from.index.mod, ta.index.mod))
                return MATCH.nomatch; // not const-compatible

            MATCH m = from.next.constConv(ta.next);
            MATCH mi = from.index.constConv(ta.index);
            if (m > MATCH.nomatch && mi > MATCH.nomatch)
            {
                return MODimplicitConv(from.mod, to.mod) ? MATCH.constant : MATCH.nomatch;
            }
        }
        return visitType(from);
    }

    /+
     + Checks whether this function type is convertible to ` to`
     + when used in a function pointer / delegate.
     +
     + Params:
     +   to = target type
     +
     + Returns:
     +   MATCH.nomatch: `to` is not a covaraint function
     +   MATCH.convert: `to` is a covaraint function
     +   MATCH.exact:   `to` is identical to this function
     +/
    MATCH implicitPointerConv(TypeFunction tf, Type to)
    {
        assert(to);

        if (tf.equals(to))
            return MATCH.constant;

        if (tf.covariant(to) == Covariant.yes)
        {
            Type tret = tf.nextOf();
            Type toret = to.nextOf();
            if (tret.ty == Tclass && toret.ty == Tclass)
            {
                /* https://issues.dlang.org/show_bug.cgi?id=10219
                 * Check covariant interface return with offset tweaking.
                 * interface I {}
                 * class C : Object, I {}
                 * I function() dg = function C() {}    // should be error
                 */
                int offset = 0;
                if (toret.isBaseOf(tret, &offset) && offset != 0)
                    return MATCH.nomatch;
            }
            return MATCH.convert;
        }

        return MATCH.nomatch;
    }

    MATCH visitPointer(TypePointer from)
    {
        //printf("TypePointer::implicitConvTo(to = %s) %s\n", to.toChars(), from.toChars());
        if (from.equals(to))
            return MATCH.exact;

        // Only convert between pointers
        auto tp = to.isTypePointer();
        if (!tp)
            return MATCH.nomatch;

        assert(from.next);
        assert(tp.next);

        // Conversion to void*
        if (tp.next.ty == Tvoid)
        {
            // Function pointer conversion doesn't check constness?
            if (from.next.ty == Tfunction)
                return MATCH.convert;

            if (!MODimplicitConv(from.next.mod, tp.next.mod))
                return MATCH.nomatch; // not const-compatible

            return from.next.ty == Tvoid ? MATCH.constant : MATCH.convert;
        }

        // Conversion between function pointers
        if (auto thisTf = from.next.isTypeFunction())
            return implicitPointerConv(thisTf, tp.next);

        // Default, no implicit conversion between the pointer targets
        MATCH m = from.next.constConv(tp.next);

        if (m == MATCH.exact && from.mod != to.mod)
            m = MATCH.constant;
        return m;
    }

    MATCH visitDelegate(TypeDelegate from)
    {
        //printf("TypeDelegate.implicitConvTo(this=%p, to=%p)\n", from, to);
        //printf("from: %s\n", from.toChars());
        //printf("to  : %s\n", to.toChars());
        if (from.equals(to))
            return MATCH.exact;

        if (auto toDg = to.isTypeDelegate())
        {
            MATCH m = implicitPointerConv(from.next.isTypeFunction(), toDg.next);

            // Retain the old behaviour for this refactoring
            // Should probably be changed to constant to match function pointers
            if (m > MATCH.convert)
                m = MATCH.convert;

            return m;
        }

        return MATCH.nomatch;
    }

    MATCH visitStruct(TypeStruct from)
    {
        //printf("TypeStruct::implicitConvTo(%s => %s)\n", from.toChars(), to.toChars());
        MATCH m = from.implicitConvToWithoutAliasThis(to);
        return m == MATCH.nomatch ? from.implicitConvToThroughAliasThis(to) : m;
    }

    MATCH visitEnum(TypeEnum from)
    {
        import dmd.enumsem : getMemtype;

        MATCH m;
        //printf("TypeEnum::implicitConvTo() %s to %s\n", from.toChars(), to.toChars());
        if (from.ty == to.ty && from.sym == (cast(TypeEnum)to).sym)
            m = (from.mod == to.mod) ? MATCH.exact : MATCH.constant;
        else if (from.sym.getMemtype(Loc.initial).implicitConvTo(to))
            m = MATCH.convert; // match with conversions
        else
            m = MATCH.nomatch; // no match
        return m;
    }

    MATCH visitClass(TypeClass from)
    {
        //printf("TypeClass::implicitConvTo(to = '%s') %s\n", to.toChars(), from.toChars());
        MATCH m = from.implicitConvToWithoutAliasThis(to);
        return m ? m : from.implicitConvToThroughAliasThis(to);
    }

    MATCH visitTuple(TypeTuple from)
    {
        if (from == to)
            return MATCH.exact;
        if (auto tt = to.isTypeTuple())
        {
            if (from.arguments.length == tt.arguments.length)
            {
                MATCH m = MATCH.exact;
                for (size_t i = 0; i < tt.arguments.length; i++)
                {
                    Parameter arg1 = (*from.arguments)[i];
                    Parameter arg2 = (*tt.arguments)[i];
                    MATCH mi = arg1.type.implicitConvTo(arg2.type);
                    if (mi < m)
                        m = mi;
                }
                return m;
            }
        }
        return MATCH.nomatch;
    }

    MATCH visitNull(TypeNull from)
    {
        //printf("TypeNull::implicitConvTo(this=%p, to=%p)\n", from, to);
        //printf("from: %s\n", from.toChars());
        //printf("to  : %s\n", to.toChars());
        MATCH m = visitType(cast(Type)from);
        if (m != MATCH.nomatch)
            return m;

        //NULL implicitly converts to any pointer type or dynamic array
        //if (type.ty == Tpointer && type.nextOf().ty == Tvoid)
        {
            Type tb = to.toBasetype();
            if (tb.ty == Tnull || tb.ty == Tpointer || tb.ty == Tarray || tb.ty == Taarray || tb.ty == Tclass || tb.ty == Tdelegate)
                return MATCH.constant;
        }

        return MATCH.nomatch;
    }

    MATCH visitNoreturn(TypeNoreturn from)
    {
        //printf("TypeNoreturn::implicitConvTo(this=%p, to=%p)\n", from, to);
        //printf("from: %s\n", from.toChars());
        //printf("to  : %s\n", to.toChars());
        if (from.equals(to))
            return MATCH.exact;

        // Different qualifiers?
        if (to.ty == Tnoreturn)
            return MATCH.constant;

        // Implicitly convertible to any type
        return MATCH.convert;
    }

    switch(from.ty)
    {
        default:             return from.isTypeBasic() ? visitBasic(from.isTypeBasic()) : visitType(from);
        case Tvector:        return visitVector(from.isTypeVector());
        case Tsarray:        return visitSArray(from.isTypeSArray());
        case Tarray:         return visitDArray(from.isTypeDArray());
        case Taarray:        return visitAArray(from.isTypeAArray());
        case Tpointer:       return visitPointer(from.isTypePointer());
        case Tdelegate:      return visitDelegate(from.isTypeDelegate());
        case Tstruct:        return visitStruct(from.isTypeStruct());
        case Tenum:          return visitEnum(from.isTypeEnum());
        case Tclass:         return visitClass(from.isTypeClass());
        case Ttuple:         return visitTuple(from.isTypeTuple());
        case Tnull:          return visitNull(from.isTypeNull());
        case Tnoreturn:      return visitNoreturn(from.isTypeNoreturn());
    }
}

/**
 * Same as implicitConvTo(); except follow C11 rules, which are quite a bit
 * more permissive than D.
 * C11 6.3 and 6.5.16.1
 * Params:
 *   e = Expression that is to be casted
 *   t = Expected resulting type
 * Returns:
 *   The `MATCH` level between `e.type` and `t`.
 */
MATCH cimplicitConvTo(Expression e, Type t)
{
    Type tb = t.toBasetype();
    Type typeb = e.type.toBasetype();

    if (tb.equals(typeb))
        return MATCH.exact;

    if (tb.isTypeVector() || typeb.isTypeVector())
        return implicitConvTo(e, t);    // permissive checking doesn't apply to vectors

    if ((typeb.isIntegral() || typeb.isFloating()) &&
        (tb.isIntegral() || tb.isFloating()))
        return MATCH.convert;
    if (tb.ty == Tpointer && typeb.isIntegral()) // C11 6.3.2.3-5
        return MATCH.convert;
    if (tb.isIntegral() && typeb.ty == Tpointer) // C11 6.3.2.3-6
        return MATCH.convert;
    if (tb.ty == Tpointer && typeb.ty == Tpointer) // C11 6.3.2.3-7
        return MATCH.convert;

    return implicitConvTo(e, t);
}

/*****************************************
 */
Type toStaticArrayType(SliceExp e)
{
    if (e.lwr && e.upr)
    {
        // For the following code to work, e should be optimized beforehand.
        // (eg. $ in lwr and upr should be already resolved, if possible)
        Expression lwr = e.lwr.optimize(WANTvalue);
        Expression upr = e.upr.optimize(WANTvalue);
        if (lwr.isConst() && upr.isConst())
        {
            size_t len = cast(size_t)(upr.toUInteger() - lwr.toUInteger());
            return e.type.toBasetype().nextOf().sarrayOf(len);
        }
    }
    else
    {
        Type t1b = e.e1.type.toBasetype();
        if (t1b.ty == Tsarray)
            return t1b;
    }
    return null;
}

/**************************************
 * Do an explicit cast.
 * Assume that the expression `e` does not have any indirections.
 * (Parameter 'att' is used to stop 'alias this' recursion)
 */
Expression castTo(Expression e, Scope* sc, Type t, Type att = null)
{
    //printf("castTo(e: %s from: %s to: %s\n", e.toChars(), e.type.toChars(), t.toChars());

    Expression visit(Expression e)
    {
        //printf("Expression::castTo(this=%s, t=%s)\n", e.toChars(), t.toChars());
        version (none)
        {
            printf("Expression::castTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        if (e.type.equals(t))
        {
            return e;
        }
        if (e.type.isTypeNoreturn() && e.op != EXP.type)
        {
            return specialNoreturnCast(e, t);
        }
        if (auto ve = e.isVarExp())
        {
            VarDeclaration v = ve.var.isVarDeclaration();
            if (v && v.storage_class & STC.manifest)
            {
                auto result = e.ctfeInterpret();
                /* https://issues.dlang.org/show_bug.cgi?id=18236
                 *
                 * The expression returned by ctfeInterpret points
                 * to the line where the manifest constant was declared
                 * so we need to update the location before trying to cast
                 */
                result.loc = e.loc;
                return result.castTo(sc, t);
            }
        }

        Type tob = t.toBasetype();
        Type t1b = e.type.toBasetype();
        if (tob.equals(t1b))
        {
            auto result = e.copy(); // because of COW for assignment to e.type
            result.type = t;
            return result;
        }

        /* Make semantic error against invalid cast between concrete types.
         * Assume that 'e' is never be any placeholder expressions.
         * The result of these checks should be consistent with CastExp::toElem().
         */

        // Fat Value types
        const(bool) tob_isFV = (tob.ty == Tstruct || tob.ty == Tsarray || tob.ty == Tvector);
        const(bool) t1b_isFV = (t1b.ty == Tstruct || t1b.ty == Tsarray || t1b.ty == Tvector);

        // Fat Reference types
        const(bool) tob_isFR = (tob.ty == Tarray || tob.ty == Tdelegate);
        const(bool) t1b_isFR = (t1b.ty == Tarray || t1b.ty == Tdelegate);

        // Reference types
        const(bool) tob_isR = (tob_isFR || tob.ty == Tpointer || tob.ty == Taarray || tob.ty == Tclass);
        const(bool) t1b_isR = (t1b_isFR || t1b.ty == Tpointer || t1b.ty == Taarray || t1b.ty == Tclass);

        // Arithmetic types (== valueable basic types)
        const(bool) tob_isA = ((tob.isIntegral() || tob.isFloating()) && tob.ty != Tvector);
        const(bool) t1b_isA = ((t1b.isIntegral() || t1b.isFloating()) && t1b.ty != Tvector);

        // Try casting the alias this member.
        // Return the expression if it succeeds, null otherwise.
        Expression tryAliasThisCast()
        {
            if (isRecursiveAliasThis(att, t1b))
                return null;

            /* Forward the cast to our alias this member, rewrite to:
             *   cast(to)e1.aliasthis
             */
            auto exp = resolveAliasThis(sc, e);
            const errors = global.startGagging();
            exp = castTo(exp, sc, t, att);
            return global.endGagging(errors) ? null : exp;
        }

        bool hasAliasThis;
        if (AggregateDeclaration t1ad = isAggregate(t1b))
        {
            AggregateDeclaration toad = isAggregate(tob);
            if (t1ad != toad && t1ad.aliasthis)
            {
                if (t1b.ty == Tclass && tob.ty == Tclass)
                {
                    ClassDeclaration t1cd = t1b.isClassHandle();
                    ClassDeclaration tocd = tob.isClassHandle();
                    int offset;
                    if (tocd.isBaseOf(t1cd, &offset))
                        goto Lok;
                }
                hasAliasThis = true;
            }
        }
        else if (tob.ty == Tvector && t1b.ty != Tvector)
        {
            if (t1b.ty == Tsarray)
            {
                // Casting static array to vector with same size, e.g. `cast(int4) int[4]`
                if (t1b.size(e.loc) != tob.size(e.loc))
                    goto Lfail;
                return new VectorExp(e.loc, e, tob).expressionSemantic(sc);
            }
            //printf("test1 e = %s, e.type = %s, tob = %s\n", e.toChars(), e.type.toChars(), tob.toChars());
            TypeVector tv = tob.isTypeVector();
            Expression result = new CastExp(e.loc, e, tv.elementType());
            result = new VectorExp(e.loc, result, tob);
            result = result.expressionSemantic(sc);
            return result;
        }
        else if (tob.ty != Tvector && t1b.ty == Tvector)
        {
            // T[n] <-- __vector(U[m])
            if (tob.ty == Tsarray)
            {
                if (t1b.size(e.loc) == tob.size(e.loc))
                    goto Lok;
            }
            goto Lfail;
        }
        else if (t1b.implicitConvTo(tob) == MATCH.constant && t.equals(e.type.constOf()))
        {
            auto result = e.copy();
            result.type = t;
            return result;
        }

        // arithmetic values vs. other arithmetic values
        // arithmetic values vs. T*
        if (tob_isA && (t1b_isA || t1b.ty == Tpointer) || t1b_isA && (tob_isA || tob.ty == Tpointer))
        {
            goto Lok;
        }

        // arithmetic values vs. references or fat values
        if (tob_isA && (t1b_isR || t1b_isFV) || t1b_isA && (tob_isR || tob_isFV))
        {
            goto Lfail;
        }

        // Bugzlla 3133: A cast between fat values is possible only when the sizes match.
        if (tob_isFV && t1b_isFV)
        {
            if (hasAliasThis)
            {
                if (auto result = tryAliasThisCast())
                    return result;
            }

            if (t1b.size(e.loc) == tob.size(e.loc))
                goto Lok;

            auto ts = toAutoQualChars(e.type, t);
            error(e.loc, "cannot cast expression `%s` of type `%s` to `%s` because of different sizes",
                e.toChars(), ts[0], ts[1]);
            return ErrorExp.get();
        }

        // Fat values vs. null or references
        if (tob_isFV && (t1b.ty == Tnull || t1b_isR) || t1b_isFV && (tob.ty == Tnull || tob_isR))
        {
            if (tob.ty == Tpointer && t1b.ty == Tsarray)
            {
                // T[n] sa;
                // cast(U*)sa; // ==> cast(U*)sa.ptr;
                return new AddrExp(e.loc, e, t);
            }
            if (tob.ty == Tarray && t1b.ty == Tsarray)
            {
                // T[n] sa;
                // cast(U[])sa; // ==> cast(U[])sa[];
                const fsize = t1b.nextOf().size();
                const tsize = tob.nextOf().size();
                if (fsize == SIZE_INVALID || tsize == SIZE_INVALID)
                {
                    return ErrorExp.get();
                }
                if (fsize != tsize)
                {
                    const dim = t1b.isTypeSArray().dim.toInteger();
                    if (tsize == 0 || (dim * fsize) % tsize != 0)
                    {
                        error(e.loc, "cannot cast expression `%s` of type `%s` to `%s` since sizes don't line up",
                                e.toChars(), e.type.toChars(), t.toChars());
                        return ErrorExp.get();
                    }
                }
                goto Lok;
            }
            goto Lfail;
        }

        /* For references, any reinterpret casts are allowed to same 'ty' type.
         *      T* to U*
         *      R1 function(P1) to R2 function(P2)
         *      R1 delegate(P1) to R2 delegate(P2)
         *      T[] to U[]
         *      V1[K1] to V2[K2]
         *      class/interface A to B  (will be a dynamic cast if possible)
         */
        if (tob.ty == t1b.ty && tob_isR && t1b_isR)
            goto Lok;

        // typeof(null) <-- non-null references or values
        if (tob.ty == Tnull && t1b.ty != Tnull)
            goto Lfail; // https://issues.dlang.org/show_bug.cgi?id=14629
        // typeof(null) --> non-null references or arithmetic values
        if (t1b.ty == Tnull && tob.ty != Tnull)
            goto Lok;

        // Check size mismatch of references.
        // Tarray and Tdelegate are (void*).sizeof*2, but others have (void*).sizeof.
        if (tob_isFR && t1b_isR || t1b_isFR && tob_isR)
        {
            if (tob.ty == Tpointer && t1b.ty == Tarray)
            {
                // T[] da;
                // cast(U*)da; // ==> cast(U*)da.ptr;
                goto Lok;
            }
            if (tob.ty == Tpointer && t1b.ty == Tdelegate)
            {
                // void delegate() dg;
                // cast(U*)dg; // ==> cast(U*)dg.ptr;
                // Note that it happens even when U is a Tfunction!
                deprecation(e.loc, "casting from %s to %s is deprecated", e.type.toChars(), t.toChars());
                goto Lok;
            }
            goto Lfail;
        }

        if (t1b.ty == Tvoid && tob.ty != Tvoid)
        {
        Lfail:
            /* if the cast cannot be performed, maybe there is an alias
             * this that can be used for casting.
             */
            if (hasAliasThis)
            {
                if (auto result = tryAliasThisCast())
                    return result;
            }
            error(e.loc, "cannot cast expression `%s` of type `%s` to `%s`", e.toChars(), e.type.toChars(), t.toChars());
            return ErrorExp.get();
        }

    Lok:
        auto result = new CastExp(e.loc, e, t);
        result.type = t; // Don't call semantic()
        //printf("Returning: %s\n", result.toChars());
        return result;
    }

    Expression visitError(ErrorExp e)
    {
        return e;
    }

    Expression visitReal(RealExp e)
    {
        if (!e.type.equals(t))
        {
            if ((e.type.isReal() && t.isReal()) || (e.type.isImaginary() && t.isImaginary()))
            {
                auto result = e.copy();
                result.type = t;
                return result;
            }
            else
                return visit(e);
        }
        return e;
    }

    Expression visitComplex(ComplexExp e)
    {
        if (!e.type.equals(t))
        {
            if (e.type.isComplex() && t.isComplex())
            {
                auto result = e.copy();
                result.type = t;
                return result;
            }
            else
                return visit(e);
        }
        return e;
    }

    Expression visitStructLiteral(StructLiteralExp e)
    {
        auto result = visit(e);
        if (auto sle = result.isStructLiteralExp())
            sle.stype = t; // commit type
        return result;
    }

    Expression visitString(StringExp e)
    {
        /* This follows copy-on-write; any changes to 'this'
         * will result in a copy.
         * The this.string member is considered immutable.
         */
        int copied = 0;

        //printf("StringExp::castTo(t = %s), '%s' committed = %d\n", t.toChars(), e.toChars(), e.committed);

        if (!e.committed && t.ty == Tpointer && t.nextOf().ty == Tvoid &&
            (!sc || !sc.inCfile))
        {
            error(e.loc, "cannot convert string literal to `void*`");
            return ErrorExp.get();
        }

        StringExp se = e;

        Expression lcast()
        {
            auto result = new CastExp(e.loc, se, t);
            result.type = t; // so semantic() won't be run on e
            return result;
        }

        if (!e.committed)
        {
            se = e.copy().isStringExp();
            se.committed = true;
            copied = 1;
        }

        if (e.type.equals(t))
        {
            return se;
        }

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (e.hexString && !e.committed && tb.nextOf().isIntegral)
        {
            const szx = cast(ubyte) tb.nextOf().size();
            if (szx != se.sz && (e.len % szx) == 0)
            {
                import dmd.utils: arrayCastBigEndian;
                const data = e.peekData();
                se.setData(arrayCastBigEndian(data, szx).ptr, data.length / szx, szx);
                se.type = t;
                return se;
            }
        }

        //printf("\ttype = %s\n", e.type.toChars());
        if (tb.ty == Tdelegate && typeb.ty != Tdelegate)
        {
            return visit(e);
        }

        if (typeb.equals(tb))
        {
            if (!copied)
            {
                se = e.copy().isStringExp();
                copied = 1;
            }
            se.type = t;
            return se;
        }

        /* Handle reinterpret casts:
         *  cast(wchar[3])"abcd"c --> [\u6261, \u6463, \u0000]
         *  cast(wchar[2])"abcd"c --> [\u6261, \u6463]
         *  cast(wchar[1])"abcd"c --> [\u6261]
         *  cast(char[4])"a" --> ['a', 0, 0, 0]
         */
        if (e.committed && tb.ty == Tsarray && typeb.ty == Tarray)
        {
            se = e.copy().isStringExp();
            uinteger_t szx = tb.nextOf().size();
            assert(szx <= 255);
            se.sz = cast(ubyte)szx;
            se.len = cast(size_t)tb.isTypeSArray().dim.toInteger();
            se.committed = true;
            se.type = t;

            /* If larger than source, pad with zeros.
             */
            const fullSize = (se.len + 1) * se.sz; // incl. terminating 0
            if (fullSize > (e.len + 1) * e.sz)
            {
                void* s = mem.xmalloc(fullSize);
                const srcSize = e.len * e.sz;
                const data = se.peekData();
                memcpy(s, data.ptr, srcSize);
                memset(s + srcSize, 0, fullSize - srcSize);
                se.setData(s, se.len, se.sz);
            }
            return se;
        }

        if (tb.ty != Tsarray && tb.ty != Tarray && tb.ty != Tpointer)
        {
            if (!copied)
            {
                se = e.copy().isStringExp();
                copied = 1;
            }
            return lcast();
        }
        if (typeb.ty != Tsarray && typeb.ty != Tarray && typeb.ty != Tpointer)
        {
            if (!copied)
            {
                se = e.copy().isStringExp();
                copied = 1;
            }
            return lcast();
        }

        const nextSz = typeb.nextOf().size();
        if (nextSz == SIZE_INVALID)
        {
            return ErrorExp.get();
        }
        if (nextSz == tb.nextOf().size())
        {
            if (!copied)
            {
                se = e.copy().isStringExp();
                copied = 1;
            }
            if (tb.ty == Tsarray)
                goto L2; // handle possible change in static array dimension
            se.type = t;
            return se;
        }

        if (e.committed)
            goto Lcast;

        static auto X(T, U)(T tf, U tt)
        {
            return cast(int)tf * 256 + cast(int)tt;
        }

        {
            OutBuffer buffer;
            size_t newlen = 0;
            int tfty = typeb.nextOf().toBasetype().ty;
            int ttty = tb.nextOf().toBasetype().ty;
            switch (X(tfty, ttty))
            {
            case X(Tchar, Tchar):
            case X(Twchar, Twchar):
            case X(Tdchar, Tdchar):
                break;

            case X(Tchar, Twchar):
                for (size_t u = 0; u < e.len;)
                {
                    dchar c;
                    if (const s = utf_decodeChar(se.peekString(), u, c))
                        error(e.loc, "%.*s", cast(int)s.length, s.ptr);
                    else
                        buffer.writeUTF16(c);
                }
                newlen = buffer.length / 2;
                buffer.writeUTF16(0);
                goto L1;

            case X(Tchar, Tdchar):
                for (size_t u = 0; u < e.len;)
                {
                    dchar c;
                    if (const s = utf_decodeChar(se.peekString(), u, c))
                        error(e.loc, "%.*s", cast(int)s.length, s.ptr);
                    buffer.write4(c);
                    newlen++;
                }
                buffer.write4(0);
                goto L1;

            case X(Twchar, Tchar):
                for (size_t u = 0; u < e.len;)
                {
                    dchar c;
                    if (const s = utf_decodeWchar(se.peekWstring(), u, c))
                        error(e.loc, "%.*s", cast(int)s.length, s.ptr);
                    else
                        buffer.writeUTF8(c);
                }
                newlen = buffer.length;
                buffer.writeUTF8(0);
                goto L1;

            case X(Twchar, Tdchar):
                for (size_t u = 0; u < e.len;)
                {
                    dchar c;
                    if (const s = utf_decodeWchar(se.peekWstring(), u, c))
                        error(e.loc, "%.*s", cast(int)s.length, s.ptr);
                    buffer.write4(c);
                    newlen++;
                }
                buffer.write4(0);
                goto L1;

            case X(Tdchar, Tchar):
                for (size_t u = 0; u < e.len; u++)
                {
                    uint c = se.peekDstring()[u];
                    if (!utf_isValidDchar(c))
                        error(e.loc, "invalid UCS-32 char \\U%08x", c);
                    else
                        buffer.writeUTF8(c);
                    newlen++;
                }
                newlen = buffer.length;
                buffer.writeUTF8(0);
                goto L1;

            case X(Tdchar, Twchar):
                for (size_t u = 0; u < e.len; u++)
                {
                    uint c = se.peekDstring()[u];
                    if (!utf_isValidDchar(c))
                        error(e.loc, "invalid UCS-32 char \\U%08x", c);
                    else
                        buffer.writeUTF16(c);
                    newlen++;
                }
                newlen = buffer.length / 2;
                buffer.writeUTF16(0);
                goto L1;

            L1:
                if (!copied)
                {
                    se = e.copy().isStringExp();
                    copied = 1;
                }

                {
                    uinteger_t szx = tb.nextOf().size();
                    assert(szx <= 255);
                    se.setData(buffer.extractSlice().ptr, newlen, cast(ubyte)szx);
                }
                break;

            default:
                assert(typeb.nextOf().size() != tb.nextOf().size());
                goto Lcast;
            }
        }
    L2:
        assert(copied);

        // See if need to truncate or extend the literal
        if (auto tsa = tb.isTypeSArray())
        {
            const dim2 = cast(size_t)tsa.dim.toInteger();
            //printf("dim from = %d, to = %d\n", cast(int)se.len, cast(int)dim2);

            // Changing dimensions
            if (dim2 != se.len)
            {
                // Copy when changing the string literal
                const newsz = se.sz;
                const d = (dim2 < se.len) ? dim2 : se.len;
                void* s = mem.xmalloc((dim2 + 1) * newsz);
                memcpy(s, se.peekData().ptr, d * newsz);
                // Extend with 0, add terminating 0
                memset(s + d * newsz, 0, (dim2 + 1 - d) * newsz);
                se.setData(s, dim2, newsz);
            }
        }
        se.type = t;
        return se;

    Lcast:
        auto result = new CastExp(e.loc, se, t);
        result.type = t; // so semantic() won't be run on e
        return result;
    }

    Expression visitAddr(AddrExp e)
    {
        version (none)
        {
            printf("AddrExp::castTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (tb.equals(typeb))
        {
            auto result = e.copy();
            result.type = t;
            return result;
        }

        // Look for pointers to functions where the functions are overloaded.
        if (e.e1.isOverExp() &&
            (tb.ty == Tpointer || tb.ty == Tdelegate) && tb.nextOf().ty == Tfunction)
        {
            OverExp eo = e.e1.isOverExp();
            FuncDeclaration f = null;
            for (size_t i = 0; i < eo.vars.a.length; i++)
            {
                auto s = eo.vars.a[i];
                auto f2 = s.isFuncDeclaration();
                assert(f2);
                if (f2.overloadExactMatch(tb.nextOf()))
                {
                    if (f)
                    {
                        /* Error if match in more than one overload set,
                         * even if one is a 'better' match than the other.
                         */
                        ScopeDsymbol.multiplyDefined(e.loc, f, f2);
                    }
                    else
                        f = f2;
                }
            }
            if (f)
            {
                f.tookAddressOf++;
                auto se = new SymOffExp(e.loc, f, 0, false);
                auto se2 = se.expressionSemantic(sc);
                // Let SymOffExp::castTo() do the heavy lifting
                return visit(se2);
            }
        }

        if (e.e1.isVarExp() &&
            typeb.ty == Tpointer && typeb.nextOf().ty == Tfunction &&
            tb.ty == Tpointer && tb.nextOf().ty == Tfunction)
        {
            auto ve = e.e1.isVarExp();
            if (auto f = ve.var.isFuncDeclaration())
            {
                assert(f.isImportedSymbol());
                f = f.overloadExactMatch(tb.nextOf());
                if (f)
                {
                    Expression result = new VarExp(e.loc, f, false);
                    result.type = f.type;
                    result = new AddrExp(e.loc, result, t);
                    return result;
                }
            }
        }

        if (auto f = isFuncAddress(e))
        {
            if (checkForwardRef(f, e.loc))
            {
                return ErrorExp.get();
            }
        }

        return visit(e);
    }

    Expression visitTuple(TupleExp e)
    {
        if (e.type.equals(t))
        {
            return e;
        }

        /* If target type is a tuple of same length, cast each expression to
         * the corresponding type in the tuple.
         */
        TypeTuple totuple;
        if (auto tt = t.isTypeTuple())
            totuple = e.exps.length == tt.arguments.length ? tt : null;

        TupleExp te = e.copy().isTupleExp();
        te.e0 = e.e0 ? e.e0.copy() : null;
        te.exps = e.exps.copy();
        for (size_t i = 0; i < te.exps.length; i++)
        {
            Expression ex = (*te.exps)[i];
            ex = ex.castTo(sc, totuple ? (*totuple.arguments)[i].type : t);
            (*te.exps)[i] = ex;
        }
        if (totuple)
            te.type = totuple;
        return te;

        /* Questionable behavior: In here, result.type is not set to t
         *  if target type is not a tuple of same length.
         * Therefoe:
         *  TypeTuple!(int, int) values;
         *  auto values2 = cast(long)values;
         *  // typeof(values2) == TypeTuple!(int, int) !!
         *
         * Only when the casted tuple is immediately expanded, it would work.
         *  auto arr = [cast(long)values];
         *  // typeof(arr) == long[]
         */
    }

    Expression visitArrayLiteral(ArrayLiteralExp e)
    {
        version (none)
        {
            printf("ArrayLiteralExp::castTo(this=%s, type=%s, => %s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }

        ArrayLiteralExp ae = e;

        Type tb = t.toBasetype();
        if (tb.ty == Tarray)
        {
            if (checkArrayLiteralEscape(*sc, ae, false))
            {
                return ErrorExp.get();
            }
        }

        if (e.type == t)
        {
            return e;
        }
        Type typeb = e.type.toBasetype();

        if ((tb.ty == Tarray || tb.ty == Tsarray) &&
            (typeb.ty == Tarray || typeb.ty == Tsarray))
        {
            if (tb.nextOf().toBasetype().ty == Tvoid && typeb.nextOf().toBasetype().ty != Tvoid)
            {
                // Don't do anything to cast non-void[] to void[]
            }
            else if (typeb.ty == Tsarray && typeb.nextOf().toBasetype().ty == Tvoid)
            {
                // Don't do anything for casting void[n] to others
            }
            else
            {
                if (auto tsa = tb.isTypeSArray())
                {
                    if (e.elements.length != tsa.dim.toInteger())
                        goto L1;
                }

                ae = e.copy().isArrayLiteralExp();
                if (e.basis)
                    ae.basis = e.basis.castTo(sc, tb.nextOf());
                ae.elements = e.elements.copy();
                for (size_t i = 0; i < e.elements.length; i++)
                {
                    Expression ex = (*e.elements)[i];
                    if (!ex)
                        continue;
                    ex = ex.castTo(sc, tb.nextOf());
                    (*ae.elements)[i] = ex;
                }
                ae.type = t;
                return ae;
            }
        }
        else if (tb.ty == Tpointer && typeb.ty == Tsarray)
        {
            Type tp = typeb.nextOf().pointerTo();
            if (!tp.equals(ae.type))
            {
                ae = e.copy().isArrayLiteralExp();
                ae.type = tp;
            }
        }
        else if (tb.ty == Tvector && (typeb.ty == Tarray || typeb.ty == Tsarray || typeb.ty == Tpointer))
        {
            // Convert array literal to vector type
            // The Tpointer case comes from C eagerly converting Tsarray to Tpointer
            TypeVector tv = tb.isTypeVector();
            TypeSArray tbase = tv.basetype.isTypeSArray();
            assert(tbase.ty == Tsarray);
            const edim = e.elements.length;
            const tbasedim = tbase.dim.toInteger();
            if (edim > tbasedim)
                goto L1;

            ae = e.copy().isArrayLiteralExp();
            ae.type = tbase; // https://issues.dlang.org/show_bug.cgi?id=12642
            ae.elements = e.elements.copy();
            Type telement = tv.elementType();
            foreach (i; 0 .. edim)
            {
                Expression ex = (*e.elements)[i];
                ex = ex.castTo(sc, telement);
                (*ae.elements)[i] = ex;
            }
            // Fill in the rest with the default initializer
            ae.elements.setDim(cast(size_t)tbasedim);
            foreach (i; edim .. cast(size_t)tbasedim)
            {
                Expression ex = typeb.nextOf.defaultInitLiteral(e.loc);
                ex = ex.castTo(sc, telement);
                (*ae.elements)[i] = ex;
            }
            Expression ev = new VectorExp(e.loc, ae, tb);
            ev = ev.expressionSemantic(sc);
            return ev;
        }
    L1:
        return visit(ae);
    }

    Expression visitAssocArrayLiteral(AssocArrayLiteralExp e)
    {
        //printf("AssocArrayLiteralExp::castTo(this=%s, type=%s, => %s)\n", e.toChars(), e.type.toChars(), t.toChars());
        if (e.type == t)
        {
            return e;
        }

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (tb.ty == Taarray && typeb.ty == Taarray &&
            tb.nextOf().toBasetype().ty != Tvoid)
        {
            AssocArrayLiteralExp ae = e.copy().isAssocArrayLiteralExp();
            ae.keys = e.keys.copy();
            ae.values = e.values.copy();
            assert(e.keys.length == e.values.length);
            for (size_t i = 0; i < e.keys.length; i++)
            {
                Expression ex = (*e.values)[i];
                ex = ex.castTo(sc, tb.nextOf());
                (*ae.values)[i] = ex;

                ex = (*e.keys)[i];
                ex = ex.castTo(sc, tb.isTypeAArray().index);
                (*ae.keys)[i] = ex;
            }
            ae.type = t;
            return ae;
        }
        return visit(e);
    }

    Expression visitSymOff(SymOffExp e)
    {
        version (none)
        {
            printf("SymOffExp::castTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        if (e.type == t && !e.hasOverloads)
        {
            return e;
        }

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (tb.equals(typeb))
        {
            auto result = e.copy();
            result.type = t;
            result.isSymOffExp().hasOverloads = false;
            return result;
        }

        // Look for pointers to functions where the functions are overloaded.
        if (e.hasOverloads &&
            typeb.ty == Tpointer && typeb.nextOf().ty == Tfunction &&
            (tb.ty == Tpointer || tb.ty == Tdelegate) && tb.nextOf().ty == Tfunction)
        {
            FuncDeclaration f = e.var.isFuncDeclaration();
            f = f ? f.overloadExactMatch(tb.nextOf()) : null;
            if (f)
            {
                Expression result;
                if (tb.ty == Tdelegate)
                {
                    if (f.needThis() && hasThis(sc))
                    {
                        result = new DelegateExp(e.loc, new ThisExp(e.loc), f, false);
                        result = result.expressionSemantic(sc);
                    }
                    else if (f.needThis())
                    {
                        error(e.loc, "no `this` to create delegate for `%s`", f.toChars());
                        return ErrorExp.get();
                    }
                    else if (f.isNested())
                    {
                        result = new DelegateExp(e.loc, IntegerExp.literal!0, f, false);
                        result = result.expressionSemantic(sc);
                    }
                    else
                    {
                        error(e.loc, "cannot cast from function pointer to delegate");
                        return ErrorExp.get();
                    }
                }
                else
                {
                    result = new SymOffExp(e.loc, f, 0, false);
                    result.type = t;
                }
                f.tookAddressOf++;
                return result;
            }
        }

        if (auto f = isFuncAddress(e))
        {
            if (checkForwardRef(f, e.loc))
            {
                return ErrorExp.get();
            }
        }

        return visit(e);
    }

    Expression visitDelegate(DelegateExp e)
    {
        version (none)
        {
            printf("DelegateExp::castTo(this=%s, type=%s, t=%s)\n", e.toChars(), e.type.toChars(), t.toChars());
        }
        static immutable msg = "cannot form delegate due to covariant return type";

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (tb.equals(typeb) && !e.hasOverloads)
        {
            int offset;
            e.func.tookAddressOf++;
            if (e.func.tintro && e.func.tintro.nextOf().isBaseOf(e.func.type.nextOf(), &offset) && offset)
                error(e.loc, "%s", msg.ptr);
            auto result = e.copy();
            result.type = t;
            return result;
        }

        // Look for delegates to functions where the functions are overloaded.
        if (typeb.ty == Tdelegate && tb.ty == Tdelegate)
        {
            if (e.func)
            {
                if (auto f = e.func.overloadExactMatch(tb.nextOf()))
                {
                    int offset;
                    if (f.tintro && f.tintro.nextOf().isBaseOf(f.type.nextOf(), &offset) && offset)
                        error(e.loc, "%s", msg.ptr);
                    if (f != e.func)    // if address not already marked as taken
                        f.tookAddressOf++;
                    auto result = new DelegateExp(e.loc, e.e1, f, false, e.vthis2);
                    result.type = t;
                    return result;
                }
                if (e.func.tintro)
                    error(e.loc, "%s", msg.ptr);
            }
        }

        if (auto f = isFuncAddress(e))
        {
            if (checkForwardRef(f, e.loc))
            {
                return ErrorExp.get();
            }
        }

        return visit(e);
    }

    Expression visitFunc(FuncExp e)
    {
        //printf("FuncExp::castTo type = %s, t = %s\n", e.type.toChars(), t.toChars());
        FuncExp fe;
        if (e.matchType(t, sc, &fe, global.errorSinkNull) > MATCH.nomatch)
        {
            return fe;
        }
        return visit(e);
    }

    Expression visitCond(CondExp e)
    {
        if (!e.type.equals(t))
        {
            auto result = new CondExp(e.loc, e.econd, e.e1.castTo(sc, t), e.e2.castTo(sc, t));
            result.type = t;
            return result;
        }
        return e;
    }

    Expression visitComma(CommaExp e)
    {
        Expression e2c = e.e2.castTo(sc, t);

        if (e2c != e.e2)
        {
            auto result = new CommaExp(e.loc, e.e1, e2c);
            result.type = e2c.type;
            return result;
        }
        else
        {
            e.type = e.e2.type;
            return e;
        }
    }

    Expression visitSlice(SliceExp e)
    {
        //printf("SliceExp::castTo e = %s, type = %s, t = %s\n", e.toChars(), e.type.toChars(), t.toChars());

        Type tb = t.toBasetype();
        Type typeb = e.type.toBasetype();

        if (e.type.equals(t) || typeb.ty != Tarray ||
            (tb.ty != Tarray && tb.ty != Tsarray))
        {
            return visit(e);
        }

        if (tb.ty == Tarray)
        {
            if (typeb.nextOf().equivalent(tb.nextOf()))
            {
                // T[] to const(T)[]
                auto result = e.copy();
                result.type = t;
                return result;
            }
            else
            {
                return visit(e);
            }
        }

        // Handle the cast from Tarray to Tsarray with CT-known slicing

        TypeSArray tsa;
        {
            Type t = toStaticArrayType(e);
            tsa = t ? t.isTypeSArray() : null;
        }

        if (tsa && tsa.size(e.loc) == tb.size(e.loc))
        {
            /* Match if the sarray sizes are equal:
             *  T[a .. b] to const(T)[b-a]
             *  T[a .. b] to U[dim] if (T.sizeof*(b-a) == U.sizeof*dim)
             *
             * If a SliceExp has Tsarray, it will become lvalue.
             * That's handled in SliceExp::isLvalue and toLvalue
             */
            auto result = e.copy();
            result.type = t;
            return result;
        }
        if (tsa && tsa.dim.equals(tb.isTypeSArray().dim))
        {
            /* Match if the dimensions are equal
             * with the implicit conversion of e.e1:
             *  cast(float[2]) [2.0, 1.0, 0.0][0..2];
             */
            Type t1b = e.e1.type.toBasetype();
            if (t1b.ty == Tsarray)
                t1b = tb.nextOf().sarrayOf(t1b.isTypeSArray().dim.toInteger());
            else if (t1b.ty == Tarray)
                t1b = tb.nextOf().arrayOf();
            else if (t1b.ty == Tpointer)
                t1b = tb.nextOf().pointerTo();
            else
                assert(0);
            if (e.e1.implicitConvTo(t1b) > MATCH.nomatch)
            {
                Expression e1x = e.e1.implicitCastTo(sc, t1b);
                assert(e1x.op != EXP.error);
                e = e.copy().isSliceExp();
                e.e1 = e1x;
                e.type = t;
                return e;
            }
        }
        auto ts = toAutoQualChars(tsa ? tsa : e.type, t);
        error(e.loc, "cannot cast expression `%s` of type `%s` to `%s`",
            e.toChars(), ts[0], ts[1]);
        return ErrorExp.get();
    }

    // Casting to noreturn isn't an actual cast
    // Rewrite cast(<qual> noreturn) <exp>
    // as      <exp>, assert(false)
    if (t.isTypeNoreturn())
    {
        // Don't generate an unreachable assert(false) if e will abort
        if (e.type.isTypeNoreturn())
        {
            // Paint e to accomodate for different type qualifiers
            e.type = t;
            return e;
        }

        auto ini = t.defaultInitLiteral(e.loc);
        return Expression.combine(e, ini);
    }

    switch (e.op)
    {
        default                   : return visit(e);
        case EXP.error            : return visitError(e.isErrorExp());
        case EXP.float64          : return visitReal(e.isRealExp());
        case EXP.complex80        : return visitComplex(e.isComplexExp());
        case EXP.structLiteral    : return visitStructLiteral(e.isStructLiteralExp());
        case EXP.string_          : return visitString(e.isStringExp());
        case EXP.address          : return visitAddr(e.isAddrExp());
        case EXP.tuple            : return visitTuple(e.isTupleExp());
        case EXP.arrayLiteral     : return visitArrayLiteral(e.isArrayLiteralExp());
        case EXP.assocArrayLiteral: return visitAssocArrayLiteral(e.isAssocArrayLiteralExp());
        case EXP.symbolOffset     : return visitSymOff(e.isSymOffExp());
        case EXP.delegate_        : return visitDelegate(e.isDelegateExp());
        case EXP.function_        : return visitFunc(e.isFuncExp());
        case EXP.question         : return visitCond(e.isCondExp());
        case EXP.comma            : return visitComma(e.isCommaExp());
        case EXP.slice            : return visitSlice(e.isSliceExp());
    }
}

/****************************************
 * Set type inference target
 *      t       Target type
 *      flag    1: don't put an error when inference fails
 */
Expression inferType(Expression e, Type t, int flag = 0)
{
    Expression visitAle(ArrayLiteralExp ale)
    {
        Type tb = t.toBasetype();
        if (tb.ty == Tarray || tb.ty == Tsarray)
        {
            Type tn = tb.nextOf();
            if (ale.basis)
                ale.basis = inferType(ale.basis, tn, flag);
            for (size_t i = 0; i < ale.elements.length; i++)
            {
                if (Expression e = (*ale.elements)[i])
                {
                    e = inferType(e, tn, flag);
                    (*ale.elements)[i] = e;
                }
            }
        }
        return ale;
    }

    Expression visitAar(AssocArrayLiteralExp aale)
    {
        Type tb = t.toBasetype();
        if (auto taa = tb.isTypeAArray())
        {
            Type ti = taa.index;
            Type tv = taa.nextOf();
            for (size_t i = 0; i < aale.keys.length; i++)
            {
                if (Expression e = (*aale.keys)[i])
                {
                    e = inferType(e, ti, flag);
                    (*aale.keys)[i] = e;
                }
            }
            for (size_t i = 0; i < aale.values.length; i++)
            {
                if (Expression e = (*aale.values)[i])
                {
                    e = inferType(e, tv, flag);
                    (*aale.values)[i] = e;
                }
            }
        }
        return aale;
    }

    Expression visitFun(FuncExp fe)
    {
        //printf("FuncExp::inferType('%s'), to=%s\n", fe.type ? fe.type.toChars() : "null", t.toChars());
        if (t.ty == Tdelegate || t.ty == Tpointer && t.nextOf().ty == Tfunction)
        {
            fe.fd.treq = t;
        }
        return fe;
    }

    Expression visitTer(CondExp ce)
    {
        Type tb = t.toBasetype();
        ce.e1 = inferType(ce.e1, tb, flag);
        ce.e2 = inferType(ce.e2, tb, flag);
        return ce;
    }

    if (t) switch (e.op)
    {
        case EXP.arrayLiteral:      return visitAle(e.isArrayLiteralExp());
        case EXP.assocArrayLiteral: return visitAar(e.isAssocArrayLiteralExp());
        case EXP.function_:         return visitFun(e.isFuncExp());
        case EXP.question:          return visitTer(e.isCondExp());
        default:
    }
    return e;
}

/****************************************
 * Scale addition/subtraction to/from pointer.
 */
Expression scaleFactor(BinExp be, Scope* sc)
{
    Type t1b = be.e1.type.toBasetype();
    Type t2b = be.e2.type.toBasetype();
    Expression eoff;

    if (t1b.ty == Tpointer && t2b.isIntegral())
    {
        // Need to adjust operator by the stride
        // Replace (ptr + int) with (ptr + (int * stride))
        Type t = Type.tptrdiff_t;

        uinteger_t stride = t1b.nextOf().size(be.loc);
        if (!t.equals(t2b))
            be.e2 = be.e2.castTo(sc, t);
        eoff = be.e2;
        be.e2 = new MulExp(be.loc, be.e2, new IntegerExp(Loc.initial, stride, t));
        be.e2.type = t;
        be.type = be.e1.type;
    }
    else if (t2b.ty == Tpointer && t1b.isIntegral())
    {
        // Need to adjust operator by the stride
        // Replace (int + ptr) with (ptr + (int * stride))
        Type t = Type.tptrdiff_t;
        Expression e;

        uinteger_t stride = t2b.nextOf().size(be.loc);
        if (!t.equals(t1b))
            e = be.e1.castTo(sc, t);
        else
            e = be.e1;
        eoff = e;
        e = new MulExp(be.loc, e, new IntegerExp(Loc.initial, stride, t));
        e.type = t;
        be.type = be.e2.type;
        be.e1 = be.e2;
        be.e2 = e;
    }
    else
        assert(0);


    eoff = eoff.optimize(WANTvalue);
    if (eoff.op == EXP.int64 && eoff.toInteger() == 0)
    {
    }
    else if (sc.setUnsafe(false, be.loc, "pointer arithmetic"))
    {
        return ErrorExp.get();
    }

    return be;
}

/**************************************
 * Return true if e is an empty array literal with dimensionality
 * equal to or less than type of other array.
 * [], [[]], [[[]]], etc.
 * I.e., make sure that [1,2] is compatible with [],
 * [[1,2]] is compatible with [[]], etc.
 */
private bool isVoidArrayLiteral(Expression e, Type other)
{
    while (e.op == EXP.arrayLiteral && e.type.ty == Tarray && (e.isArrayLiteralExp().elements.length == 1))
    {
        auto ale = e.isArrayLiteralExp();
        e = ale[0];
        if (other.ty == Tsarray || other.ty == Tarray)
            other = other.nextOf();
        else
            return false;
    }
    if (other.ty != Tsarray && other.ty != Tarray)
        return false;
    Type t = e.type;
    return (e.op == EXP.arrayLiteral && t.ty == Tarray && t.nextOf().ty == Tvoid && e.isArrayLiteralExp().elements.length == 0);
}

/**
 * Merge types of `e1` and `e2` into a common subset
 *
 * Parameters `e1` and `e2` will be rewritten in place as needed.
 *
 * Params:
 *     sc  = Current scope
 *     op  = Operator such as `e1 op e2`. In practice, either EXP.question
 *           or one of the binary operator.
 *     pe1 = The LHS of the operation, will be rewritten
 *     pe2 = The RHS of the operation, will be rewritten
 *
 * Returns:
 *      The resulting type in case of success, `null` in case of error
 */
Type typeMerge(Scope* sc, EXP op, ref Expression pe1, ref Expression pe2)
{
    //printf("typeMerge() %s op %s\n", pe1.toChars(), pe2.toChars());

    Expression e1 = pe1;
    Expression e2 = pe2;

    // ImportC: do array/function conversions
    if (sc)
    {
        e1 = e1.arrayFuncConv(sc);
        e2 = e2.arrayFuncConv(sc);
    }

    Type Lret(Type result)
    {
        pe1 = e1;
        pe2 = e2;

        version (none)
        {
            printf("-typeMerge() %s op %s\n", e1.toChars(), e2.toChars());
            if (e1.type)
                printf("\tt1 = %s\n", e1.type.toChars());
            if (e2.type)
                printf("\tt2 = %s\n", e2.type.toChars());
            printf("\ttype = %s\n", result.toChars());
        }
        return result;
    }

    /// Converts one of the expression to the other
    Type convert(ref Expression from, Type to)
    {
        from = from.castTo(sc, to);
        return Lret(to);
    }

    /// Converts both expression to a third type
    Type coerce(Type towards)
    {
        e1 = e1.castTo(sc, towards);
        e2 = e2.castTo(sc, towards);
        return Lret(towards);
    }

    Type t1b = e1.type.toBasetype();
    Type t2b = e2.type.toBasetype();

    if (sc && sc.inCfile)
    {
        // Integral types can be implicitly converted to pointers
        if ((t1b.ty == Tpointer) != (t2b.ty == Tpointer))
        {
            if (t1b.isIntegral())
            {
                return convert(e1, t2b);
            }
            else if (t2b.isIntegral())
            {
                return convert(e2, t1b);
            }
        }
    }

    if (op != EXP.question || t1b.ty != t2b.ty && (t1b.isTypeBasic() && t2b.isTypeBasic()))
    {
        if (op == EXP.question && t1b.ty.isSomeChar() && t2b.ty.isSomeChar())
        {
            e1 = e1.castTo(sc, Type.tdchar);
            e2 = e2.castTo(sc, Type.tdchar);
        }
        else
        {
            e1 = integralPromotions(e1, sc);
            e2 = integralPromotions(e2, sc);
        }
    }

    MATCH m;
    Type t1 = e1.type;
    Type t2 = e2.type;
    assert(t1);
    Type t = t1;

    /* The start type of alias this type recursion.
     * In following case, we should save A, and stop recursion
     * if it appears again.
     *      X -> Y -> [A] -> B -> A -> B -> ...
     */
    Type att1 = null;
    Type att2 = null;

    if (t1.mod != t2.mod &&
        t1.ty == Tenum && t2.ty == Tenum &&
        t1.isTypeEnum().sym == t2.isTypeEnum().sym)
    {
        ubyte mod = MODmerge(t1.mod, t2.mod);
        t1 = t1.castMod(mod);
        t2 = t2.castMod(mod);
        return Lret(t1);
    }

Lagain:
    t1b = t1.toBasetype();
    t2b = t2.toBasetype();

    const ty = implicitConvCommonTy(t1b.ty, t2b.ty);
    if (ty != Terror)
    {
        const ty1 = implicitConvTy1(t1b.ty, t2b.ty);
        const ty2 = implicitConvTy1(t2b.ty, t1b.ty);

        if (t1b.ty == ty1) // if no promotions
        {
            if (t1.equals(t2))
                return Lret(t1);

            if (t1b.equals(t2b))
                return Lret(t1b);
        }

        t1 = Type.basic[ty1];
        t2 = Type.basic[ty2];

        if (!(t1 && t2))
            return null;
        e1 = e1.castTo(sc, t1);
        e2 = e2.castTo(sc, t2);
        return Lret(Type.basic[ty]);
    }

    t1 = t1b;
    t2 = t2b;

    if (t1.ty == Ttuple || t2.ty == Ttuple)
        return null;

    if (t1.equals(t2))
    {
        // merging can not result in new enum type
        if (t.ty == Tenum)
            return Lret(t1b);
        return Lret(t);
    }

    if ((t1.ty == Tpointer && t2.ty == Tpointer) || (t1.ty == Tdelegate && t2.ty == Tdelegate))
    {
        // Bring pointers to compatible type
        Type t1n = t1.nextOf();
        Type t2n = t2.nextOf();

        if (t1n.equals(t2n))
            return Lret(t);

        if (t1n.ty == Tvoid) // pointers to void are always compatible
            return Lret(t1);

        if (t2n.ty == Tvoid)
            return Lret(t2);

        if (t1.implicitConvTo(t2))
            return convert(e1, t2);

        if (t2.implicitConvTo(t1))
            return convert(e2, t1);

        if (t1n.ty == Tfunction && t2n.ty == Tfunction)
        {
            TypeFunction tf1 = t1n.isTypeFunction();
            TypeFunction tf2 = t2n.isTypeFunction();
            tf1.purityLevel();
            tf2.purityLevel();

            TypeFunction d = tf1.syntaxCopy();

            if (tf1.purity != tf2.purity)
                d.purity = PURE.impure;
            assert(d.purity != PURE.fwdref);

            d.isNothrow = (tf1.isNothrow && tf2.isNothrow);
            d.isNogc = (tf1.isNogc && tf2.isNogc);

            if (tf1.trust == tf2.trust)
                d.trust = tf1.trust;
            else if (tf1.trust <= TRUST.system || tf2.trust <= TRUST.system)
                d.trust = TRUST.system;
            else
                d.trust = TRUST.trusted;

            Type tx = (t1.ty == Tdelegate) ? new TypeDelegate(d) : d.pointerTo();
            tx = tx.typeSemantic(e1.loc, sc);

            if (t1.implicitConvTo(tx) && t2.implicitConvTo(tx))
                return coerce(tx);
            return null;
        }

        if (t1n.mod != t2n.mod)
        {
            if (!t1n.isImmutable() && !t2n.isImmutable() && t1n.isShared() != t2n.isShared())
                return null;
            ubyte mod = MODmerge(t1n.mod, t2n.mod);
            t1 = t1n.castMod(mod).pointerTo();
            t2 = t2n.castMod(mod).pointerTo();
            t = t1;
            goto Lagain;
        }

        if (t1n.ty == Tclass && t2n.ty == Tclass)
        {
            ClassDeclaration cd1 = t1n.isClassHandle();
            ClassDeclaration cd2 = t2n.isClassHandle();
            int offset;
            if (cd1.isBaseOf(cd2, &offset))
            {
                if (offset)
                    e2 = e2.castTo(sc, t);
                return Lret(t);
            }

            if (cd2.isBaseOf(cd1, &offset))
            {
                if (offset)
                    e1 = e1.castTo(sc, t2);
                return Lret(t2);
            }

            return null;
        }

        t1 = t1n.constOf().pointerTo();
        t2 = t2n.constOf().pointerTo();
        if (t1.implicitConvTo(t2))
            return convert(e1, t2);
        if (t2.implicitConvTo(t1))
            return convert(e2, t1);
        return null;
    }

    if ((t1.ty == Tsarray || t1.ty == Tarray) && (e2.op == EXP.null_ && t2.ty == Tpointer && t2.nextOf().ty == Tvoid || e2.op == EXP.arrayLiteral && t2.ty == Tsarray && t2.nextOf().ty == Tvoid && t2.isTypeSArray().dim.toInteger() == 0 || isVoidArrayLiteral(e2, t1)))
    {
        /*  (T[n] op void*)   => T[]
         *  (T[]  op void*)   => T[]
         *  (T[n] op void[0]) => T[]
         *  (T[]  op void[0]) => T[]
         *  (T[n] op void[])  => T[]
         *  (T[]  op void[])  => T[]
         */
        return coerce(t1.nextOf().arrayOf());
    }

    if ((t2.ty == Tsarray || t2.ty == Tarray) && (e1.op == EXP.null_ && t1.ty == Tpointer && t1.nextOf().ty == Tvoid || e1.op == EXP.arrayLiteral && t1.ty == Tsarray && t1.nextOf().ty == Tvoid && t1.isTypeSArray().dim.toInteger() == 0 || isVoidArrayLiteral(e1, t2)))
    {
        /*  (void*   op T[n]) => T[]
         *  (void*   op T[])  => T[]
         *  (void[0] op T[n]) => T[]
         *  (void[0] op T[])  => T[]
         *  (void[]  op T[n]) => T[]
         *  (void[]  op T[])  => T[]
         */
        return coerce(t2.nextOf().arrayOf());
    }

    if ((t1.ty == Tsarray || t1.ty == Tarray) && (m = t1.implicitConvTo(t2)) != MATCH.nomatch)
    {
        // https://issues.dlang.org/show_bug.cgi?id=7285
        // Tsarray op [x, y, ...] should to be Tsarray
        // https://issues.dlang.org/show_bug.cgi?id=14737
        // Tsarray ~ [x, y, ...] should to be Tarray
        if (t1.ty == Tsarray && e2.op == EXP.arrayLiteral && op != EXP.concatenate)
            return convert(e2, t1);
        if (m == MATCH.constant && (op == EXP.addAssign || op == EXP.minAssign || op == EXP.mulAssign || op == EXP.divAssign || op == EXP.modAssign || op == EXP.powAssign || op == EXP.andAssign || op == EXP.orAssign || op == EXP.xorAssign))
        {
            // Don't make the lvalue const
            return Lret(t2);
        }
        return convert(e1, t2);
    }

    if ((t2.ty == Tsarray || t2.ty == Tarray) && t2.implicitConvTo(t1))
    {
        // https://issues.dlang.org/show_bug.cgi?id=7285
        // https://issues.dlang.org/show_bug.cgi?id=14737
        if (t2.ty == Tsarray && e1.op == EXP.arrayLiteral && op != EXP.concatenate)
            return convert(e1, t2);
        return convert(e2, t1);
    }

    if ((t1.ty == Tsarray || t1.ty == Tarray || t1.ty == Tpointer) && (t2.ty == Tsarray || t2.ty == Tarray || t2.ty == Tpointer) && t1.nextOf().mod != t2.nextOf().mod)
    {
        /* If one is mutable and the other immutable, then retry
         * with both of them as const
         */
        Type t1n = t1.nextOf();
        Type t2n = t2.nextOf();
        ubyte mod;
        if (e1.op == EXP.null_ && e2.op != EXP.null_)
            mod = t2n.mod;
        else if (e1.op != EXP.null_ && e2.op == EXP.null_)
            mod = t1n.mod;
        else if (!t1n.isImmutable() && !t2n.isImmutable() && t1n.isShared() != t2n.isShared())
            return null;
        else
            mod = MODmerge(t1n.mod, t2n.mod);

        if (t1.ty == Tpointer)
            t1 = t1n.castMod(mod).pointerTo();
        else
            t1 = t1n.castMod(mod).arrayOf();

        if (t2.ty == Tpointer)
            t2 = t2n.castMod(mod).pointerTo();
        else
            t2 = t2n.castMod(mod).arrayOf();
        t = t1;
        goto Lagain;
    }

LmergeClassTypes:
    /* Merge different type modifiers on classes
     */
    if (t1.ty == Tclass && t2.ty == Tclass)
    {
        if (t1.mod != t2.mod)
        {
            ubyte mod;
            if (e1.op == EXP.null_ && e2.op != EXP.null_)
                mod = t2.mod;
            else if (e1.op != EXP.null_ && e2.op == EXP.null_)
                mod = t1.mod;
            else if (!t1.isImmutable() && !t2.isImmutable() && t1.isShared() != t2.isShared())
                return null;
            else
                mod = MODmerge(t1.mod, t2.mod);
            t1 = t1.castMod(mod);
            t2 = t2.castMod(mod);
            t = t1;
            goto Lagain;
        }
        goto Lcc;
    }

    if (t1.ty == Tclass || t2.ty == Tclass)
    {
    Lcc:
        while (1)
        {
            MATCH i1woat = MATCH.exact;
            MATCH i2woat = MATCH.exact;

            if (auto t2c = t2.isTypeClass())
                i1woat = t2c.implicitConvToWithoutAliasThis(t1);
            if (auto t1c = t1.isTypeClass())
                i2woat = t1c.implicitConvToWithoutAliasThis(t2);

            MATCH i1 = e2.implicitConvTo(t1);
            MATCH i2 = e1.implicitConvTo(t2);

            if (i1 && i2)
            {
                // We have the case of class vs. void*, so pick class
                if (t1.ty == Tpointer)
                    i1 = MATCH.nomatch;
                else if (t2.ty == Tpointer)
                    i2 = MATCH.nomatch;
            }

            // Match but without 'alias this' on classes
            if (i2 && i2woat)
                return coerce(t2);
            if (i1 && i1woat)
                return coerce(t1);

            // Here use implicitCastTo() instead of castTo() to try 'alias this' on classes
            Type coerceImplicit(Type towards)
            {
                e1 = e1.implicitCastTo(sc, towards);
                e2 = e2.implicitCastTo(sc, towards);
                return Lret(towards);
            }

            // Implicit conversion with 'alias this'
            if (i2)
                return coerceImplicit(t2);
            if (i1)
                return coerceImplicit(t1);

            if (t1.ty == Tclass && t2.ty == Tclass)
            {
                /* t1 cannot be converted to t2, and vice versa
                 */
                TypeClass tc1 = t1.isTypeClass();
                TypeClass tc2 = t2.isTypeClass();

                //if (tc1.sym.interfaces.length || tc2.sym.interfaces.length)
                if (tc1.sym.isInterfaceDeclaration() ||
                    tc2.sym.isInterfaceDeclaration())
                {
                    ClassDeclaration cd = findClassCommonRoot(tc1.sym, tc2.sym);
                    if (!cd)
                        return null;    // no common root
                    t1 = cd.type.castMod(t1.mod);
                    t2 = cd.type.castMod(t2.mod);
                    goto LmergeClassTypes;   // deal with mod differences
                }

                /* Pick 'tightest' type
                 */
                ClassDeclaration cd1 = tc1.sym.baseClass;
                ClassDeclaration cd2 = tc2.sym.baseClass;
                if (cd1 && cd2)
                {
                    t1 = cd1.type.castMod(t1.mod);
                    t2 = cd2.type.castMod(t2.mod);
                }
                else if (cd1)
                    t1 = cd1.type;
                else if (cd2)
                    t2 = cd2.type;
                else
                    return null;
                goto LmergeClassTypes;
            }
            else if (t1.ty == Tstruct && t1.isTypeStruct().sym.aliasthis)
            {
                if (isRecursiveAliasThis(att1, e1.type))
                    return null;
                //printf("att tmerge(c || c) e1 = %s\n", e1.type.toChars());
                e1 = resolveAliasThis(sc, e1);
                t1 = e1.type;
                continue;
            }
            else if (t2.ty == Tstruct && t2.isTypeStruct().sym.aliasthis)
            {
                if (isRecursiveAliasThis(att2, e2.type))
                    return null;
                //printf("att tmerge(c || c) e2 = %s\n", e2.type.toChars());
                e2 = resolveAliasThis(sc, e2);
                t2 = e2.type;
                continue;
            }
            else
                return null;
        }
    }

    if (t1.ty == Tstruct && t2.ty == Tstruct)
    {
        if (t1.mod != t2.mod)
        {
            if (!t1.isImmutable() && !t2.isImmutable() && t1.isShared() != t2.isShared())
                return null;
            ubyte mod = MODmerge(t1.mod, t2.mod);
            t1 = t1.castMod(mod);
            t2 = t2.castMod(mod);
            t = t1;
            goto Lagain;
        }

        TypeStruct ts1 = t1.isTypeStruct();
        TypeStruct ts2 = t2.isTypeStruct();
        if (ts1.sym != ts2.sym)
        {
            if (!ts1.sym.aliasthis && !ts2.sym.aliasthis)
                return null;

            MATCH i1 = MATCH.nomatch;
            MATCH i2 = MATCH.nomatch;

            Expression e1b = null;
            Expression e2b = null;
            if (ts2.sym.aliasthis)
            {
                if (isRecursiveAliasThis(att2, e2.type))
                    return null;
                //printf("att tmerge(s && s) e2 = %s\n", e2.type.toChars());
                e2b = resolveAliasThis(sc, e2);
                i1 = e2b.implicitConvTo(t1);
            }
            if (ts1.sym.aliasthis)
            {
                if (isRecursiveAliasThis(att1, e1.type))
                    return null;
                //printf("att tmerge(s && s) e1 = %s\n", e1.type.toChars());
                e1b = resolveAliasThis(sc, e1);
                i2 = e1b.implicitConvTo(t2);
            }
            if (i1 && i2)
                return null;

            if (i1)
                return convert(e2, t1);
            if (i2)
                return convert(e1, t2);

            if (e1b)
            {
                e1 = e1b;
                t1 = e1b.type.toBasetype();
            }
            if (e2b)
            {
                e2 = e2b;
                t2 = e2b.type.toBasetype();
            }
            t = t1;
            goto Lagain;
        }
    }

    if (t1.ty == Tstruct && t1.isTypeStruct().sym.aliasthis)
    {
        if (isRecursiveAliasThis(att1, e1.type))
            return null;
        //printf("att tmerge(s || s) e1 = %s\n", e1.type.toChars());
        e1 = resolveAliasThis(sc, e1);
        t1 = e1.type;
        t = t1;
        goto Lagain;
    }

    if (t2.ty == Tstruct && t2.isTypeStruct().sym.aliasthis)
    {
        if (isRecursiveAliasThis(att2, e2.type))
            return null;
        //printf("att tmerge(s || s) e2 = %s\n", e2.type.toChars());
        e2 = resolveAliasThis(sc, e2);
        t2 = e2.type;
        t = t2;
        goto Lagain;
    }

    if ((e1.op == EXP.string_ || e1.op == EXP.null_) && e1.implicitConvTo(t2))
        return convert(e1, t2);
    if ((e2.op == EXP.string_ || e2.op == EXP.null_) && e2.implicitConvTo(t1))
        return convert(e2, t1);
    if (t1.ty == Tsarray && t2.ty == Tsarray && e2.implicitConvTo(t1.nextOf().arrayOf()))
        return coerce(t1.nextOf().arrayOf());
    if (t1.ty == Tsarray && t2.ty == Tsarray && e1.implicitConvTo(t2.nextOf().arrayOf()))
        return coerce(t2.nextOf().arrayOf());

    if (t1.ty == Tvector && t2.ty == Tvector)
    {
        // https://issues.dlang.org/show_bug.cgi?id=13841
        // all vector types should have no common types between
        // different vectors, even though their sizes are same.
        auto tv1 = t1.isTypeVector();
        auto tv2 = t2.isTypeVector();
        if (!tv1.basetype.equals(tv2.basetype))
            return null;

        goto LmodCompare;
    }

    if (t1.ty == Tvector && t2.ty != Tvector && e2.implicitConvTo(t1))
    {
        e2 = e2.castTo(sc, t1);
        t2 = t1;
        t = t1;
        goto Lagain;
    }

    if (t2.ty == Tvector && t1.ty != Tvector && e1.implicitConvTo(t2))
    {
        e1 = e1.castTo(sc, t2);
        t1 = t2;
        t = t1;
        goto Lagain;
    }

    if (t1.isIntegral() && t2.isIntegral())
    {
        if (t1.ty != t2.ty)
        {
            if (t1.ty == Tvector || t2.ty == Tvector)
                return null;
            e1 = integralPromotions(e1, sc);
            e2 = integralPromotions(e2, sc);
            t1 = e1.type;
            t2 = e2.type;
            goto Lagain;
        }
        assert(t1.ty == t2.ty);
LmodCompare:
        if (!t1.isImmutable() && !t2.isImmutable() && t1.isShared() != t2.isShared())
            return null;
        ubyte mod = MODmerge(t1.mod, t2.mod);

        t1 = t1.castMod(mod);
        t2 = t2.castMod(mod);
        t = t1;
        e1 = e1.castTo(sc, t);
        e2 = e2.castTo(sc, t);
        goto Lagain;
    }

    if (t1.ty == Tnull && t2.ty == Tnull)
    {
        ubyte mod = MODmerge(t1.mod, t2.mod);
        return coerce(t1.castMod(mod));
    }

    if (t2.ty == Tnull && (t1.ty == Tpointer || t1.ty == Taarray || t1.ty == Tarray))
        return convert(e2, t1);
    if (t1.ty == Tnull && (t2.ty == Tpointer || t2.ty == Taarray || t2.ty == Tarray))
        return convert(e1, t2);

    /// Covers array operations for user-defined types
    Type checkArrayOpType(Expression e1, Expression e2, EXP op, Scope *sc)
    {
        // scalar op scalar - we shouldn't be here
        if (e1.type.ty != Tarray && e1.type.ty != Tsarray && e2.type.ty != Tarray && e2.type.ty != Tsarray)
            return null;

        // only supporting slices and array literals
        if (!e1.isSliceExp() && !e1.isArrayLiteralExp() && !e2.isSliceExp() && !e2.isArrayLiteralExp())
            return null;

        // start with e1 op e2 and if either one of e1 or e2 is a slice or array literal,
        // replace it with the first element of the array
        Expression lhs = e1;
        Expression rhs = e2;

        // T[x .. y] op ?
        if (auto se1 = e1.isSliceExp())
            lhs = new IndexExp(Loc.initial, se1.e1, IntegerExp.literal!0);

        // [t1, t2, .. t3] op ?
        if (auto ale1 = e1.isArrayLiteralExp())
            lhs = ale1.opIndex(0);

        // ? op U[z .. t]
        if (auto se2 = e2.isSliceExp())
            rhs = new IndexExp(Loc.initial, se2.e1, IntegerExp.literal!0);

        // ? op [u1, u2, .. u3]
        if (auto ale2 = e2.isArrayLiteralExp())
            rhs = ale2.opIndex(0);

        // create a new binary expression with the new lhs and rhs (at this stage, at least
        // one of lhs/rhs has been replaced with the 0'th element of the array it was before)
        Expression exp;
        switch (op)
        {
            case EXP.add:
                exp = new AddExp(Loc.initial, lhs, rhs); break;
            case EXP.min:
                exp = new MinExp(Loc.initial, lhs, rhs); break;
            case EXP.mul:
                exp = new MulExp(Loc.initial, lhs, rhs); break;
            case EXP.div:
                exp = new DivExp(Loc.initial, lhs, rhs); break;
            case EXP.pow:
                exp = new PowExp(Loc.initial, lhs, rhs); break;
            default:
                exp = null;
        }

        if (exp)
        {
            // if T op U is valid and has type V
            // then T[] op U and T op U[] should be valid and have type V[]
            Expression e = exp.trySemantic(sc);
            if (e && e.type)
                return e.type.arrayOf;
        }

        return null;
    }

    if (t1.ty == Tarray && isBinArrayOp(op) && isArrayOpOperand(e1))
    {
        if (e2.implicitConvTo(t1.nextOf()))
        {
            // T[] op T
            // T[] op cast(T)U
            e2 = e2.castTo(sc, t1.nextOf());
            return Lret(t1.nextOf().arrayOf());
        }
        if (t1.nextOf().implicitConvTo(e2.type))
        {
            // (cast(T)U)[] op T    (https://issues.dlang.org/show_bug.cgi?id=12780)
            // e1 is left as U[], it will be handled in arrayOp() later.
            return Lret(e2.type.arrayOf());
        }
        if (t2.ty == Tarray && isArrayOpOperand(e2))
        {
            if (t1.nextOf().implicitConvTo(t2.nextOf()))
            {
                // (cast(T)U)[] op T[]  (https://issues.dlang.org/show_bug.cgi?id=12780)
                t = t2.nextOf().arrayOf();
                // if cast won't be handled in arrayOp() later
                if (!isArrayOpImplicitCast(t1.isTypeDArray(), t2.isTypeDArray()))
                    e1 = e1.castTo(sc, t);
                return Lret(t);
            }
            if (t2.nextOf().implicitConvTo(t1.nextOf()))
            {
                // T[] op (cast(T)U)[]  (https://issues.dlang.org/show_bug.cgi?id=12780)
                // e2 is left as U[], it will be handled in arrayOp() later.
                t = t1.nextOf().arrayOf();
                // if cast won't be handled in arrayOp() later
                if (!isArrayOpImplicitCast(t2.isTypeDArray(), t1.isTypeDArray()))
                    e2 = e2.castTo(sc, t);
                return Lret(t);
            }
        }

        t = checkArrayOpType(e1, e2, op, sc);
        if (t !is null)
            return Lret(t);

        return null;
    }
    else if (t2.ty == Tarray && isBinArrayOp(op) && isArrayOpOperand(e2))
    {
        if (e1.implicitConvTo(t2.nextOf()))
        {
            // T op T[]
            // cast(T)U op T[]
            e1 = e1.castTo(sc, t2.nextOf());
            t = t2.nextOf().arrayOf();
        }
        else if (t2.nextOf().implicitConvTo(e1.type))
        {
            // T op (cast(T)U)[]    (https://issues.dlang.org/show_bug.cgi?id=12780)
            // e2 is left as U[], it will be handled in arrayOp() later.
            t = e1.type.arrayOf();
        }
        else
        {
            t = checkArrayOpType(e1, e2, op, sc);
            if (t is null)
                return null;
        }

        //printf("test %s\n", EXPtoString(op).ptr);
        e1 = e1.optimize(WANTvalue);
        if (isCommutative(op) && e1.isConst())
        {
            /* Swap operands to minimize number of functions generated
             */
            //printf("swap %s\n", EXPtoString(op).ptr);
            Expression tmp = e1;
            e1 = e2;
            e2 = tmp;
        }
        return Lret(t);
    }

    return null;
}

/**********************************
 * Find common root that both cd1 and cd2 can be implicitly converted to.
 * Params:
 *      cd1 = first class
 *      cd2 = second class
 * Returns:
 *      common base that both can implicitly convert to, null if none or
 *      multiple matches
 */
private
ClassDeclaration findClassCommonRoot(ClassDeclaration cd1, ClassDeclaration cd2)
{
    enum log = false;
    if (log) printf("findClassCommonRoot(%s, %s)\n", cd1.toChars(), cd2.toChars());
    /* accumulate results in this */
    static struct Root
    {
        ClassDeclaration cd;
        bool error;

        /* merge cd into results */
        void accumulate(ClassDeclaration cd)
        {
            if (log) printf(" accumulate(r.cd: %s r.error: %d cd: %s)\n", this.cd ? this.cd.toChars() : "null", error, cd ? cd.toChars() : null);
            if (this.cd is cd)
            {
            }
            else if (!this.cd)
                this.cd = cd;
            else
                error = true;
        }
    }

    /* Find common root of cd1 and cd2. Accumulate results in r. depth is nesting level */
    void findCommonRoot(ClassDeclaration cd1, ClassDeclaration cd2, ref Root r)
    {
        if (log) printf("findCommonRoot(cd1: %s cd2: %s)\n", cd1.toChars(), cd2.toChars());
        /* Warning: quadratic time function
         */
        if (cd1 is cd2)
        {
            r.accumulate(cd1);
            return;
        }

        foreach (b1; (*cd1.baseclasses)[])
        {
            if (b1.sym != r.cd)
                findCommonRoot(cd2, b1.sym, r);
        }
        foreach (b2; (*cd2.baseclasses)[])
        {
            if (b2.sym != r.cd)
                findCommonRoot(cd1, b2.sym, r);
        }
    }

    Root r;
    findCommonRoot(cd1, cd2, r);
    if (!r.cd || r.error)
        return null;        // no common root
    return r.cd;
}

/************************************
 * Bring leaves to common type.
 * Returns:
 *    null on success, ErrorExp if error occurs
 */
Expression typeCombine(BinExp be, Scope* sc)
{
    Expression errorReturn()
    {
        Expression ex = be.incompatibleTypes();
        if (ex.op == EXP.error)
            return ex;
        return ErrorExp.get();
    }

    Type t1 = be.e1.type.toBasetype();
    Type t2 = be.e2.type.toBasetype();

    if (be.op == EXP.min || be.op == EXP.add)
    {
        // struct+struct, and class+class are errors
        if (t1.ty == Tstruct && t2.ty == Tstruct)
            return errorReturn();
        if (t1.ty == Tclass && t2.ty == Tclass)
            return errorReturn();
        if (t1.ty == Taarray && t2.ty == Taarray)
            return errorReturn();
    }

    if (auto result = typeMerge(sc, be.op, be.e1, be.e2))
    {
        if (be.type is null)
            be.type = result;
    }
    else
        return errorReturn();

    // If the types have no value, return an error
    if (be.e1.op == EXP.error)
        return be.e1;
    if (be.e2.op == EXP.error)
        return be.e2;
    return null;
}

/***********************************
 * Do integral promotions (convertchk).
 * Don't convert <array of> to <pointer to>
 */
Expression integralPromotions(Expression e, Scope* sc)
{
    //printf("integralPromotions %s %s\n", e.toChars(), e.type.toChars());
    switch (e.type.toBasetype().ty)
    {
    case Tvoid:
        error(e.loc, "void has no value");
        return ErrorExp.get();

    case Tint8:
    case Tuns8:
    case Tint16:
    case Tuns16:
    case Tbool:
    case Tchar:
    case Twchar:
        e = e.castTo(sc, Type.tint32);
        break;

    case Tdchar:
        e = e.castTo(sc, Type.tuns32);
        break;

    default:
        break;
    }
    return e;
}

/******************************************************
 * This provides a transition from the non-promoting behavior
 * of unary + - ~ to the C-like integral promotion behavior.
 * Params:
 *    sc = context
 *    ue = NegExp, UAddExp, or ComExp which is revised per rules
 * References:
 *      https://issues.dlang.org/show_bug.cgi?id=16997
 */

void fix16997(Scope* sc, UnaExp ue)
{
    if (global.params.fix16997 || sc.inCfile)
        ue.e1 = integralPromotions(ue.e1, sc);          // desired C-like behavor
    else
    {
        switch (ue.e1.type.toBasetype.ty)
        {
            case Tint8:
            case Tuns8:
            case Tint16:
            case Tuns16:
            //case Tbool:       // these operations aren't allowed on bool anyway
            case Tchar:
            case Twchar:
            case Tdchar:
                deprecation(ue.loc, "integral promotion not done for `%s`, remove '-revert=intpromote' switch or `%scast(int)(%s)`",
                    ue.toChars(), EXPtoString(ue.op).ptr, ue.e1.toChars());
                break;

            default:
                break;
        }
    }
}

/***********************************
 * See if both types are arrays that can be compared
 * for equality without any casting. Return true if so.
 * This is to enable comparing things like an immutable
 * array with a mutable one.
 */
extern (D) bool arrayTypeCompatibleWithoutCasting(Type t1, Type t2)
{
    t1 = t1.toBasetype();
    t2 = t2.toBasetype();

    if ((t1.ty == Tarray || t1.ty == Tsarray || t1.ty == Tpointer) && t2.ty == t1.ty)
    {
        if (t1.nextOf().implicitConvTo(t2.nextOf()) >= MATCH.constant || t2.nextOf().implicitConvTo(t1.nextOf()) >= MATCH.constant)
            return true;
    }
    return false;
}

/******************************************************************/
/* Determine the integral ranges of an expression.
 * This is used to determine if implicit narrowing conversions will
 * be allowed.
 */
@trusted
IntRange getIntRange(Expression e)
{
    IntRange visit(Expression e)
    {
        return IntRange.fromType(e.type);
    }

    IntRange visitInteger(IntegerExp e)
    {
        return IntRange(SignExtendedNumber(e.getInteger()))._cast(e.type);
    }

    IntRange visitCast(CastExp e)
    {
        return getIntRange(e.e1)._cast(e.type);
    }

    IntRange visitAdd(AddExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);
        return (ir1 + ir2)._cast(e.type);
    }

    IntRange visitMin(MinExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);
        return (ir1 - ir2)._cast(e.type);
    }

    IntRange visitDiv(DivExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);

        return (ir1 / ir2)._cast(e.type);
    }

    IntRange visitMul(MulExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);

        return (ir1 * ir2)._cast(e.type);
    }

    IntRange visitMod(ModExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);

        // Modding on 0 is invalid anyway.
        if (!ir2.absNeg().imin.negative)
        {
            return visit(e);
        }
        return (ir1 % ir2)._cast(e.type);
    }

    IntRange visitAnd(AndExp e)
    {
        IntRange result;
        bool hasResult = false;
        result.unionOrAssign(getIntRange(e.e1) & getIntRange(e.e2), hasResult);

        assert(hasResult);
        return result._cast(e.type);
    }

    IntRange visitOr(OrExp e)
    {
        IntRange result;
        bool hasResult = false;
        result.unionOrAssign(getIntRange(e.e1) | getIntRange(e.e2), hasResult);

        assert(hasResult);
        return result._cast(e.type);
    }

    IntRange visitXor(XorExp e)
    {
        IntRange result;
        bool hasResult = false;
        result.unionOrAssign(getIntRange(e.e1) ^ getIntRange(e.e2), hasResult);

        assert(hasResult);
        return result._cast(e.type);
    }

    IntRange visitShl(ShlExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);

        return (ir1 << ir2)._cast(e.type);
    }

    IntRange visitShr(ShrExp e)
    {
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);

        return (ir1 >> ir2)._cast(e.type);
    }

    IntRange visitUshr(UshrExp e)
    {
        IntRange ir1 = getIntRange(e.e1).castUnsigned(e.e1.type);
        IntRange ir2 = getIntRange(e.e2);

        return (ir1 >>> ir2)._cast(e.type);
    }

    IntRange visitAssign(AssignExp e)
    {
        return getIntRange(e.e2)._cast(e.type);
    }

    IntRange visitCond(CondExp e)
    {
        // No need to check e.econd; assume caller has called optimize()
        IntRange ir1 = getIntRange(e.e1);
        IntRange ir2 = getIntRange(e.e2);
        return ir1.unionWith(ir2)._cast(e.type);
    }

    IntRange visitVar(VarExp e)
    {
        Expression ie;
        VarDeclaration vd = e.var.isVarDeclaration();
        if (vd && vd.range)
            return vd.range._cast(e.type);
        if (vd && vd._init && !vd.type.isMutable() && (ie = vd.getConstInitializer()) !is null)
            return getIntRange(ie);
        return visit(e);
    }

    IntRange visitComma(CommaExp e)
    {
        return getIntRange(e.e2);
    }

    IntRange visitCom(ComExp e)
    {
        IntRange ir = getIntRange(e.e1);
        return IntRange(SignExtendedNumber(~ir.imax.value, !ir.imax.negative), SignExtendedNumber(~ir.imin.value, !ir.imin.negative))._cast(e.type);
    }

    IntRange visitNeg(NegExp e)
    {
        IntRange ir = getIntRange(e.e1);
        return (-ir)._cast(e.type);
    }

    switch (e.op)
    {
        default                     : return visit(e);
        case EXP.int64              : return visitInteger(e.isIntegerExp());
        case EXP.cast_              : return visitCast(e.isCastExp());
        case EXP.add                : return visitAdd(e.isAddExp());
        case EXP.min                : return visitMin(e.isMinExp());
        case EXP.div                : return visitDiv(e.isDivExp());
        case EXP.mul                : return visitMul(e.isMulExp());
        case EXP.mod                : return visitMod(e.isModExp());
        case EXP.and                : return visitAnd(e.isAndExp());
        case EXP.or                 : return visitOr(e.isOrExp());
        case EXP.xor                : return visitXor(e.isXorExp());
        case EXP.leftShift          : return visitShl(e.isShlExp());
        case EXP.rightShift         : return visitShr(e.isShrExp());
        case EXP.unsignedRightShift : return visitUshr(e.isUshrExp());
        case EXP.blit               : return visitAssign(e.isBlitExp());
        case EXP.construct          : return visitAssign(e.isConstructExp());
        case EXP.assign             : return visitAssign(e.isAssignExp());
        case EXP.question           : return visitCond(e.isCondExp());
        case EXP.variable           : return visitVar(e.isVarExp());
        case EXP.comma              : return visitComma(e.isCommaExp());
        case EXP.tilde              : return visitCom(e.isComExp());
        case EXP.negate             : return visitNeg(e.isNegExp());
    }
}
/**
 * A helper function to "cast" from expressions of type noreturn to
 * any other type - noreturn is implicitly convertible to any other type.
 * However, the dmd backend does not like a naive cast from a noreturn expression
 * (particularly an `assert(0)`) so this function generates:
 *
 * `(assert(0), value)` instead of `cast(to)(assert(0))`.
 *
 * `value` is currently `to.init` however it cannot be read so could be made simpler.
 * Params:
 *   toBeCasted = Expression of type noreturn to cast
 *   to = Type to cast the expression to.
 * Returns: A CommaExp, upon any failure ErrorExp will be returned.
 */
Expression specialNoreturnCast(Expression toBeCasted, Type to)
{
    return Expression.combine(toBeCasted, to.defaultInitLiteral(toBeCasted.loc));
}
