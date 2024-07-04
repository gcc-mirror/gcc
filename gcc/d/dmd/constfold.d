/**
 * Perform constant folding of arithmetic expressions.
 *
 * The routines in this module are called from `optimize.d`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/float.html#fp_const_folding, Floating Point Constant Folding)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/constfold.d, _constfold.d)
 * Documentation:  https://dlang.org/phobos/dmd_constfold.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/constfold.d
 */

module dmd.constfold;

import core.stdc.string;
import core.stdc.stdio;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ctfeexpr;
import dmd.dcast;
import dmd.declaration;
import dmd.dstruct;
import dmd.errors;
import dmd.expression;
import dmd.globals;
import dmd.location;
import dmd.mtype;
import dmd.root.complex;
import dmd.root.ctfloat;
import dmd.root.port;
import dmd.root.rmem;
import dmd.root.utf;
import dmd.sideeffect;
import dmd.target;
import dmd.tokens;
import dmd.typesem : toDsymbol, equivalent, sarrayOf;

private enum LOG = false;

private Expression expType(Type type, Expression e)
{
    if (type != e.type)
    {
        e = e.copy();
        e.type = type;
    }
    return e;
}

/**********************************
 * Initialize a EXP.cantExpression Expression.
 * Params:
 *      ue = where to write it
 */
void cantExp(out UnionExp ue)
{
    emplaceExp!(CTFEExp)(&ue, EXP.cantExpression);
}

/* =============================== constFold() ============================== */
/* The constFold() functions were redundant with the optimize() ones,
 * and so have been folded in with them.
 */
/* ========================================================================== */
UnionExp Neg(Type type, Expression e1)
{
    UnionExp ue = void;
    Loc loc = e1.loc;
    if (e1.type.isreal())
    {
        emplaceExp!(RealExp)(&ue, loc, -e1.toReal(), type);
    }
    else if (e1.type.isimaginary())
    {
        emplaceExp!(RealExp)(&ue, loc, -e1.toImaginary(), type);
    }
    else if (e1.type.iscomplex())
    {
        emplaceExp!(ComplexExp)(&ue, loc, -e1.toComplex(), type);
    }
    else
    {
        emplaceExp!(IntegerExp)(&ue, loc, -e1.toInteger(), type);
    }
    return ue;
}

UnionExp Com(Type type, Expression e1)
{
    UnionExp ue = void;
    Loc loc = e1.loc;
    emplaceExp!(IntegerExp)(&ue, loc, ~e1.toInteger(), type);
    return ue;
}

UnionExp Not(Type type, Expression e1)
{
    UnionExp ue = void;
    Loc loc = e1.loc;
    // BUG: Should be replaced with e1.toBool().get(), but this is apparently
    //      executed for some expressions that cannot be const-folded
    //      To be fixed in another PR
    emplaceExp!(IntegerExp)(&ue, loc, e1.toBool().hasValue(false) ? 1 : 0, type);
    return ue;
}

UnionExp Add(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    static if (LOG)
    {
        printf("Add(e1 = %s, e2 = %s)\n", e1.toChars(), e2.toChars());
    }
    if (type.isreal())
    {
        emplaceExp!(RealExp)(&ue, loc, e1.toReal() + e2.toReal(), type);
    }
    else if (type.isimaginary())
    {
        emplaceExp!(RealExp)(&ue, loc, e1.toImaginary() + e2.toImaginary(), type);
    }
    else if (type.iscomplex())
    {
        // This rigamarole is necessary so that -0.0 doesn't get
        // converted to +0.0 by doing an extraneous add with +0.0
        auto c1 = complex_t(CTFloat.zero);
        real_t r1 = CTFloat.zero;
        real_t i1 = CTFloat.zero;
        auto c2 = complex_t(CTFloat.zero);
        real_t r2 = CTFloat.zero;
        real_t i2 = CTFloat.zero;
        auto v = complex_t(CTFloat.zero);
        int x;
        if (e1.type.isreal())
        {
            r1 = e1.toReal();
            x = 0;
        }
        else if (e1.type.isimaginary())
        {
            i1 = e1.toImaginary();
            x = 3;
        }
        else
        {
            c1 = e1.toComplex();
            x = 6;
        }
        if (e2.type.isreal())
        {
            r2 = e2.toReal();
        }
        else if (e2.type.isimaginary())
        {
            i2 = e2.toImaginary();
            x += 1;
        }
        else
        {
            c2 = e2.toComplex();
            x += 2;
        }
        switch (x)
        {
        case 0 + 0:
            v = complex_t(r1 + r2);
            break;
        case 0 + 1:
            v = complex_t(r1, i2);
            break;
        case 0 + 2:
            v = complex_t(r1 + creall(c2), cimagl(c2));
            break;
        case 3 + 0:
            v = complex_t(r2, i1);
            break;
        case 3 + 1:
            v = complex_t(CTFloat.zero, i1 + i2);
            break;
        case 3 + 2:
            v = complex_t(creall(c2), i1 + cimagl(c2));
            break;
        case 6 + 0:
            v = complex_t(creall(c1) + r2, cimagl(c2));
            break;
        case 6 + 1:
            v = complex_t(creall(c1), cimagl(c1) + i2);
            break;
        case 6 + 2:
            v = c1 + c2;
            break;
        default:
            assert(0);
        }
        emplaceExp!(ComplexExp)(&ue, loc, v, type);
    }
    else if (SymOffExp soe = e1.isSymOffExp())
    {
        emplaceExp!(SymOffExp)(&ue, loc, soe.var, soe.offset + e2.toInteger());
        ue.exp().type = type;
    }
    else if (SymOffExp soe = e2.isSymOffExp())
    {
        emplaceExp!(SymOffExp)(&ue, loc, soe.var, soe.offset + e1.toInteger());
        ue.exp().type = type;
    }
    else
        emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger() + e2.toInteger(), type);
    return ue;
}

UnionExp Min(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    // Compute e1-e2 as e1+(-e2)
    UnionExp neg = Neg(e2.type, e2);
    UnionExp ue = Add(loc, type, e1, neg.exp());
    return ue;
}

UnionExp Mul(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    if (type.isfloating())
    {
        auto c = complex_t(CTFloat.zero);
        real_t r = CTFloat.zero;
        if (e1.type.isreal())
        {
            r = e1.toReal();
            c = e2.toComplex();
            c = complex_t(r * creall(c), r * cimagl(c));
        }
        else if (e1.type.isimaginary())
        {
            r = e1.toImaginary();
            c = e2.toComplex();
            c = complex_t(-r * cimagl(c), r * creall(c));
        }
        else if (e2.type.isreal())
        {
            r = e2.toReal();
            c = e1.toComplex();
            c = complex_t(r * creall(c), r * cimagl(c));
        }
        else if (e2.type.isimaginary())
        {
            r = e2.toImaginary();
            c = e1.toComplex();
            c = complex_t(-r * cimagl(c), r * creall(c));
        }
        else
            c = e1.toComplex() * e2.toComplex();
        if (type.isreal())
            emplaceExp!(RealExp)(&ue, loc, creall(c), type);
        else if (type.isimaginary())
            emplaceExp!(RealExp)(&ue, loc, cimagl(c), type);
        else if (type.iscomplex())
            emplaceExp!(ComplexExp)(&ue, loc, c, type);
        else
            assert(0);
    }
    else
    {
        emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger() * e2.toInteger(), type);
    }
    return ue;
}

UnionExp Div(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    if (type.isfloating())
    {
        auto c = complex_t(CTFloat.zero);
        if (e2.type.isreal())
        {
            if (e1.type.isreal())
            {
                emplaceExp!(RealExp)(&ue, loc, e1.toReal() / e2.toReal(), type);
                return ue;
            }
            const r = e2.toReal();
            c = e1.toComplex();
            c = complex_t(creall(c) / r, cimagl(c) / r);
        }
        else if (e2.type.isimaginary())
        {
            const r = e2.toImaginary();
            c = e1.toComplex();
            c = complex_t(cimagl(c) / r, -creall(c) / r);
        }
        else
        {
            c = e1.toComplex() / e2.toComplex();
        }

        if (type.isreal())
            emplaceExp!(RealExp)(&ue, loc, creall(c), type);
        else if (type.isimaginary())
            emplaceExp!(RealExp)(&ue, loc, cimagl(c), type);
        else if (type.iscomplex())
            emplaceExp!(ComplexExp)(&ue, loc, c, type);
        else
            assert(0);
    }
    else
    {
        sinteger_t n1;
        sinteger_t n2;
        sinteger_t n;
        n1 = e1.toInteger();
        n2 = e2.toInteger();
        if (n2 == 0)
        {
            error(e2.loc, "divide by 0");
            emplaceExp!(ErrorExp)(&ue);
            return ue;
        }
        if (n2 == -1 && !type.isunsigned())
        {
            // Check for int.min / -1
            if (n1 == 0xFFFFFFFF80000000UL && type.toBasetype().ty != Tint64)
            {
                error(e2.loc, "integer overflow: `int.min / -1`");
                emplaceExp!(ErrorExp)(&ue);
                return ue;
            }
            else if (n1 == 0x8000000000000000L) // long.min / -1
            {
                error(e2.loc, "integer overflow: `long.min / -1L`");
                emplaceExp!(ErrorExp)(&ue);
                return ue;
            }
        }
        if (e1.type.isunsigned() || e2.type.isunsigned())
            n = (cast(dinteger_t)n1) / (cast(dinteger_t)n2);
        else
            n = n1 / n2;
        emplaceExp!(IntegerExp)(&ue, loc, n, type);
    }
    return ue;
}

UnionExp Mod(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    if (type.isfloating())
    {
        auto c = complex_t(CTFloat.zero);
        if (e2.type.isreal())
        {
            const r2 = e2.toReal();
            c = complex_t(e1.toReal() % r2, e1.toImaginary() % r2);
        }
        else if (e2.type.isimaginary())
        {
            const i2 = e2.toImaginary();
            c = complex_t(e1.toReal() % i2, e1.toImaginary() % i2);
        }
        else
            assert(0);
        if (type.isreal())
            emplaceExp!(RealExp)(&ue, loc, creall(c), type);
        else if (type.isimaginary())
            emplaceExp!(RealExp)(&ue, loc, cimagl(c), type);
        else if (type.iscomplex())
            emplaceExp!(ComplexExp)(&ue, loc, c, type);
        else
            assert(0);
    }
    else
    {
        sinteger_t n1;
        sinteger_t n2;
        sinteger_t n;
        n1 = e1.toInteger();
        n2 = e2.toInteger();
        if (n2 == 0)
        {
            error(e2.loc, "divide by 0");
            emplaceExp!(ErrorExp)(&ue);
            return ue;
        }
        if (n2 == -1 && !type.isunsigned())
        {
            // Check for int.min % -1
            if (n1 == 0xFFFFFFFF80000000UL && type.toBasetype().ty != Tint64)
            {
                error(e2.loc, "integer overflow: `int.min %% -1`");
                emplaceExp!(ErrorExp)(&ue);
                return ue;
            }
            else if (n1 == 0x8000000000000000L) // long.min % -1
            {
                error(e2.loc, "integer overflow: `long.min %% -1L`");
                emplaceExp!(ErrorExp)(&ue);
                return ue;
            }
        }
        if (e1.type.isunsigned() || e2.type.isunsigned())
            n = (cast(dinteger_t)n1) % (cast(dinteger_t)n2);
        else
            n = n1 % n2;
        emplaceExp!(IntegerExp)(&ue, loc, n, type);
    }
    return ue;
}

UnionExp Pow(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    //printf("Pow()\n");
    UnionExp ue;
    // Handle integer power operations.
    if (e2.type.isintegral())
    {
        dinteger_t n = e2.toInteger();
        bool neg;
        if (!e2.type.isunsigned() && cast(sinteger_t)n < 0)
        {
            if (e1.type.isintegral())
            {
                cantExp(ue);
                return ue;
            }
            // Don't worry about overflow, from now on n is unsigned.
            neg = true;
            n = -n;
        }
        else
            neg = false;
        UnionExp ur, uv;
        if (e1.type.iscomplex())
        {
            emplaceExp!(ComplexExp)(&ur, loc, e1.toComplex(), e1.type);
            emplaceExp!(ComplexExp)(&uv, loc, complex_t(CTFloat.one), e1.type);
        }
        else if (e1.type.isfloating())
        {
            emplaceExp!(RealExp)(&ur, loc, e1.toReal(), e1.type);
            emplaceExp!(RealExp)(&uv, loc, CTFloat.one, e1.type);
        }
        else
        {
            emplaceExp!(IntegerExp)(&ur, loc, e1.toInteger(), e1.type);
            emplaceExp!(IntegerExp)(&uv, loc, 1, e1.type);
        }
        Expression r = ur.exp();
        Expression v = uv.exp();
        while (n != 0)
        {
            if (n & 1)
            {
                // v = v * r;
                uv = Mul(loc, v.type, v, r);
            }
            n >>= 1;
            // r = r * r
            ur = Mul(loc, r.type, r, r);
        }
        if (neg)
        {
            // ue = 1.0 / v
            UnionExp one;
            emplaceExp!(RealExp)(&one, loc, CTFloat.one, v.type);
            uv = Div(loc, v.type, one.exp(), v);
        }
        if (type.iscomplex())
            emplaceExp!(ComplexExp)(&ue, loc, v.toComplex(), type);
        else if (type.isintegral())
            emplaceExp!(IntegerExp)(&ue, loc, v.toInteger(), type);
        else
            emplaceExp!(RealExp)(&ue, loc, v.toReal(), type);
    }
    else if (e2.type.isfloating())
    {
        // x ^^ y for x < 0 and y not an integer is not defined; so set result as NaN
        if (e1.toReal() < CTFloat.zero)
        {
            emplaceExp!(RealExp)(&ue, loc, target.RealProperties.nan, type);
        }
        else
            cantExp(ue);
    }
    else
        cantExp(ue);
    return ue;
}

UnionExp Shl(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger() << e2.toInteger(), type);
    return ue;
}

UnionExp Shr(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    dinteger_t value = e1.toInteger();
    dinteger_t dcount = e2.toInteger();
    assert(dcount <= 0xFFFFFFFF);
    uint count = cast(uint)dcount;
    switch (e1.type.toBasetype().ty)
    {
    case Tint8:
        value = cast(byte)value >> count;
        break;
    case Tuns8:
    case Tchar:
        value = cast(ubyte)value >> count;
        break;
    case Tint16:
        value = cast(short)value >> count;
        break;
    case Tuns16:
    case Twchar:
        value = cast(ushort)value >> count;
        break;
    case Tint32:
        value = cast(int)value >> count;
        break;
    case Tuns32:
    case Tdchar:
        value = cast(uint)value >> count;
        break;
    case Tint64:
        value = cast(long)value >> count;
        break;
    case Tuns64:
        value = cast(ulong)value >> count;
        break;
    case Terror:
        emplaceExp!(ErrorExp)(&ue);
        return ue;
    default:
        assert(0);
    }
    emplaceExp!(IntegerExp)(&ue, loc, value, type);
    return ue;
}

UnionExp Ushr(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    dinteger_t value = e1.toInteger();
    dinteger_t dcount = e2.toInteger();
    assert(dcount <= 0xFFFFFFFF);
    uint count = cast(uint)dcount;
    switch (e1.type.toBasetype().ty)
    {
    case Tint8:
    case Tuns8:
    case Tchar:
        // Possible only with >>>=. >>> always gets promoted to int.
        value = (value & 0xFF) >>> count;
        break;
    case Tint16:
    case Tuns16:
    case Twchar:
        // Possible only with >>>=. >>> always gets promoted to int.
        value = (value & 0xFFFF) >>> count;
        break;
    case Tint32:
    case Tuns32:
    case Tdchar:
        value = (value & 0xFFFFFFFF) >>> count;
        break;
    case Tint64:
    case Tuns64:
        value = value >>> count;
        break;
    case Terror:
        emplaceExp!(ErrorExp)(&ue);
        return ue;
    default:
        assert(0);
    }
    emplaceExp!(IntegerExp)(&ue, loc, value, type);
    return ue;
}

UnionExp And(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger() & e2.toInteger(), type);
    return ue;
}

UnionExp Or(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger() | e2.toInteger(), type);
    return ue;
}

UnionExp Xor(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    //printf("Xor(linnum = %d, e1 = %s, e2 = %s)\n", loc.linnum, e1.toChars(), e2.toChars());
    UnionExp ue = void;
    emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger() ^ e2.toInteger(), type);
    return ue;
}

/* Also returns EXP.cantExpression if cannot be computed.
 */
UnionExp Equal(EXP op, const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    int cmp = 0;
    real_t r1 = CTFloat.zero;
    real_t r2 = CTFloat.zero;
    //printf("Equal(e1 = %s, e2 = %s)\n", e1.toChars(), e2.toChars());
    assert(op == EXP.equal || op == EXP.notEqual);
    if (e1.op == EXP.null_)
    {
        if (e2.op == EXP.null_)
            cmp = 1;
        else if (StringExp es2 = e2.isStringExp())
        {
            cmp = (0 == es2.len);
        }
        else if (ArrayLiteralExp es2 = e2.isArrayLiteralExp())
        {
            cmp = !es2.elements || (0 == es2.elements.length);
        }
        else
        {
            cantExp(ue);
            return ue;
        }
    }
    else if (e2.op == EXP.null_)
    {
        if (StringExp es1 = e1.isStringExp())
        {
            cmp = (0 == es1.len);
        }
        else if (ArrayLiteralExp es1 = e1.isArrayLiteralExp())
        {
            cmp = !es1.elements || (0 == es1.elements.length);
        }
        else
        {
            cantExp(ue);
            return ue;
        }
    }
    else if (e1.op == EXP.string_ && e2.op == EXP.string_)
    {
        StringExp es1 = e1.isStringExp();
        StringExp es2 = e2.isStringExp();
        if (es1.sz != es2.sz)
        {
            assert(global.errors);
            cantExp(ue);
            return ue;
        }
        const data1 = es1.peekData();
        const data2 = es2.peekData();
        if (es1.len == es2.len && memcmp(data1.ptr, data2.ptr, es1.sz * es1.len) == 0)
            cmp = 1;
        else
            cmp = 0;
    }
    else if (e1.op == EXP.arrayLiteral && e2.op == EXP.arrayLiteral)
    {
        ArrayLiteralExp es1 = e1.isArrayLiteralExp();
        ArrayLiteralExp es2 = e2.isArrayLiteralExp();
        if ((!es1.elements || !es1.elements.length) && (!es2.elements || !es2.elements.length))
            cmp = 1; // both arrays are empty
        else if (!es1.elements || !es2.elements)
            cmp = 0;
        else if (es1.elements.length != es2.elements.length)
            cmp = 0;
        else
        {
            for (size_t i = 0; i < es1.elements.length; i++)
            {
                auto ee1 = es1[i];
                auto ee2 = es2[i];
                ue = Equal(EXP.equal, loc, Type.tint32, ee1, ee2);
                if (CTFEExp.isCantExp(ue.exp()))
                    return ue;
                cmp = cast(int)ue.exp().toInteger();
                if (cmp == 0)
                    break;
            }
        }
    }
    else if (e1.op == EXP.arrayLiteral && e2.op == EXP.string_)
    {
        // Swap operands and use common code
        Expression etmp = e1;
        e1 = e2;
        e2 = etmp;
        goto Lsa;
    }
    else if (e1.op == EXP.string_ && e2.op == EXP.arrayLiteral)
    {
    Lsa:
        StringExp es1 = e1.isStringExp();
        ArrayLiteralExp es2 = e2.isArrayLiteralExp();
        size_t dim1 = es1.len;
        size_t dim2 = es2.elements ? es2.elements.length : 0;
        if (dim1 != dim2)
            cmp = 0;
        else
        {
            cmp = 1; // if dim1 winds up being 0
            foreach (i; 0 .. dim1)
            {
                uinteger_t c = es1.getIndex(i);
                auto ee2 = es2[i];
                if (ee2.isConst() != 1)
                {
                    cantExp(ue);
                    return ue;
                }
                cmp = (c == ee2.toInteger());
                if (cmp == 0)
                    break;
            }
        }
    }
    else if (e1.op == EXP.structLiteral && e2.op == EXP.structLiteral)
    {
        StructLiteralExp es1 = e1.isStructLiteralExp();
        StructLiteralExp es2 = e2.isStructLiteralExp();
        if (es1.sd != es2.sd)
            cmp = 0;
        else if ((!es1.elements || !es1.elements.length) && (!es2.elements || !es2.elements.length))
            cmp = 1; // both arrays are empty
        else if (!es1.elements || !es2.elements)
            cmp = 0;
        else if (es1.elements.length != es2.elements.length)
            cmp = 0;
        else
        {
            cmp = 1;
            for (size_t i = 0; i < es1.elements.length; i++)
            {
                Expression ee1 = (*es1.elements)[i];
                Expression ee2 = (*es2.elements)[i];
                if (ee1 == ee2)
                    continue;
                if (!ee1 || !ee2)
                {
                    cmp = 0;
                    break;
                }
                ue = Equal(EXP.equal, loc, Type.tint32, ee1, ee2);
                if (ue.exp().op == EXP.cantExpression)
                    return ue;
                cmp = cast(int)ue.exp().toInteger();
                if (cmp == 0)
                    break;
            }
        }
    }
    else if (e1.isConst() != 1 || e2.isConst() != 1)
    {
        cantExp(ue);
        return ue;
    }
    else if (e1.type.isreal())
    {
        r1 = e1.toReal();
        r2 = e2.toReal();
        goto L1;
    }
    else if (e1.type.isimaginary())
    {
        r1 = e1.toImaginary();
        r2 = e2.toImaginary();
    L1:
        if (CTFloat.isNaN(r1) || CTFloat.isNaN(r2)) // if unordered
        {
            cmp = 0;
        }
        else
        {
            cmp = (r1 == r2);
        }
    }
    else if (e1.type.iscomplex())
    {
        cmp = e1.toComplex() == e2.toComplex();
    }
    else if (e1.type.isintegral() || e1.type.toBasetype().ty == Tpointer)
    {
        cmp = (e1.toInteger() == e2.toInteger());
    }
    else
    {
        cantExp(ue);
        return ue;
    }
    if (op == EXP.notEqual)
        cmp ^= 1;
    emplaceExp!(IntegerExp)(&ue, loc, cmp, type);
    return ue;
}

UnionExp Identity(EXP op, const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    int cmp;
    if (e1.op == EXP.null_)
    {
        cmp = (e2.op == EXP.null_);
    }
    else if (e2.op == EXP.null_)
    {
        cmp = 0;
    }
    else if (e1.op == EXP.symbolOffset && e2.op == EXP.symbolOffset)
    {
        SymOffExp es1 = e1.isSymOffExp();
        SymOffExp es2 = e2.isSymOffExp();
        cmp = (es1.var == es2.var && es1.offset == es2.offset);
    }
    else
    {
        if (e1.type.isfloating())
            cmp = e1.isIdentical(e2);
        else
        {
            ue = Equal((op == EXP.identity) ? EXP.equal : EXP.notEqual, loc, type, e1, e2);
            return ue;
        }
    }
    if (op == EXP.notIdentity)
        cmp ^= 1;
    emplaceExp!(IntegerExp)(&ue, loc, cmp, type);
    return ue;
}

UnionExp Cmp(EXP op, const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    dinteger_t n;
    real_t r1 = CTFloat.zero;
    real_t r2 = CTFloat.zero;
    //printf("Cmp(e1 = %s, e2 = %s)\n", e1.toChars(), e2.toChars());
    if (e1.op == EXP.string_ && e2.op == EXP.string_)
    {
        StringExp es1 = e1.isStringExp();
        StringExp es2 = e2.isStringExp();
        size_t sz = es1.sz;
        assert(sz == es2.sz);
        size_t len = es1.len;
        if (es2.len < len)
            len = es2.len;
        const data1 = es1.peekData();
        const data2 = es1.peekData();
        int rawCmp = memcmp(data1.ptr, data2.ptr, sz * len);
        if (rawCmp == 0)
            rawCmp = cast(int)(es1.len - es2.len);
        n = specificCmp(op, rawCmp);
    }
    else if (e1.isConst() != 1 || e2.isConst() != 1)
    {
        cantExp(ue);
        return ue;
    }
    else if (e1.type.isreal())
    {
        r1 = e1.toReal();
        r2 = e2.toReal();
        goto L1;
    }
    else if (e1.type.isimaginary())
    {
        r1 = e1.toImaginary();
        r2 = e2.toImaginary();
    L1:
        n = realCmp(op, r1, r2);
    }
    else if (e1.type.iscomplex())
    {
        assert(0);
    }
    else
    {
        sinteger_t n1;
        sinteger_t n2;
        n1 = e1.toInteger();
        n2 = e2.toInteger();
        if (e1.type.isunsigned() || e2.type.isunsigned())
            n = intUnsignedCmp(op, n1, n2);
        else
            n = intSignedCmp(op, n1, n2);
    }
    emplaceExp!(IntegerExp)(&ue, loc, n, type);
    return ue;
}

/* Also returns EXP.cantExpression if cannot be computed.
 *  to: type to cast to
 *  type: type to paint the result
 */
UnionExp Cast(const ref Loc loc, Type type, Type to, Expression e1)
{
    UnionExp ue = void;
    Type tb = to.toBasetype();
    Type typeb = type.toBasetype();
    //printf("Cast(type = %s, to = %s, e1 = %s)\n", type.toChars(), to.toChars(), e1.toChars());
    //printf("\te1.type = %s\n", e1.type.toChars());
    if (e1.type.equals(type) && type.equals(to))
    {
        emplaceExp!(UnionExp)(&ue, e1);
        return ue;
    }
    if (e1.op == EXP.vector && (cast(TypeVector)e1.type).basetype.equals(type) && type.equals(to))
    {
        Expression ex = e1.isVectorExp().e1;
        emplaceExp!(UnionExp)(&ue, ex);
        return ue;
    }
    if (e1.type.toBasetype.equals(type) && type.equals(to))
    {
        emplaceExp!(UnionExp)(&ue, e1);
        ue.exp().type = type;
        return ue;
    }
    if (e1.type.implicitConvTo(to) >= MATCH.constant || to.implicitConvTo(e1.type) >= MATCH.constant)
    {
        goto L1;
    }
    // Allow covariant converions of delegates
    // (Perhaps implicit conversion from pure to impure should be a MATCH.constant,
    // then we wouldn't need this extra check.)
    if (e1.type.toBasetype().ty == Tdelegate && e1.type.implicitConvTo(to) == MATCH.convert)
    {
        goto L1;
    }
    /* Allow casting from one string type to another
     */
    if (e1.op == EXP.string_)
    {
        if (tb.ty == Tarray && typeb.ty == Tarray && tb.nextOf().size() == typeb.nextOf().size())
        {
            goto L1;
        }
    }
    if (e1.op == EXP.arrayLiteral && typeb == tb)
    {
    L1:
        Expression ex = expType(to, e1);
        emplaceExp!(UnionExp)(&ue, ex);
        return ue;
    }
    if (e1.isConst() != 1)
    {
        cantExp(ue);
    }
    else if (tb.ty == Tbool)
    {
        const opt = e1.toBool();
        if (opt.isEmpty())
        {
            cantExp(ue);
            return ue;
        }

        emplaceExp!(IntegerExp)(&ue, loc, opt.get(), type);
    }
    else if (type.isintegral())
    {
        if (e1.type.isfloating())
        {
            dinteger_t result;
            real_t r = e1.toReal();
            switch (typeb.ty)
            {
            case Tint8:
                result = cast(byte)cast(sinteger_t)r;
                break;
            case Tchar:
            case Tuns8:
                result = cast(ubyte)cast(dinteger_t)r;
                break;
            case Tint16:
                result = cast(short)cast(sinteger_t)r;
                break;
            case Twchar:
            case Tuns16:
                result = cast(ushort)cast(dinteger_t)r;
                break;
            case Tint32:
                result = cast(int)r;
                break;
            case Tdchar:
            case Tuns32:
                result = cast(uint)r;
                break;
            case Tint64:
                result = cast(long)r;
                break;
            case Tuns64:
                result = cast(ulong)r;
                break;
            default:
                assert(0);
            }
            emplaceExp!(IntegerExp)(&ue, loc, result, type);
        }
        else if (type.isunsigned())
            emplaceExp!(IntegerExp)(&ue, loc, e1.toUInteger(), type);
        else
            emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger(), type);
    }
    else if (tb.isreal())
    {
        real_t value = e1.toReal();
        emplaceExp!(RealExp)(&ue, loc, value, type);
    }
    else if (tb.isimaginary())
    {
        real_t value = e1.toImaginary();
        emplaceExp!(RealExp)(&ue, loc, value, type);
    }
    else if (tb.iscomplex())
    {
        complex_t value = e1.toComplex();
        emplaceExp!(ComplexExp)(&ue, loc, value, type);
    }
    else if (tb.isscalar())
    {
        emplaceExp!(IntegerExp)(&ue, loc, e1.toInteger(), type);
    }
    else if (tb.ty == Tvoid)
    {
        cantExp(ue);
    }
    else if (tb.ty == Tstruct && e1.op == EXP.int64)
    {
        // Struct = 0;
        StructDeclaration sd = tb.toDsymbol(null).isStructDeclaration();
        assert(sd);
        auto elements = new Expressions();
        for (size_t i = 0; i < sd.fields.length; i++)
        {
            VarDeclaration v = sd.fields[i];
            UnionExp zero;
            emplaceExp!(IntegerExp)(&zero, 0);
            ue = Cast(loc, v.type, v.type, zero.exp());
            if (ue.exp().op == EXP.cantExpression)
                return ue;
            elements.push(ue.exp().copy());
        }
        emplaceExp!(StructLiteralExp)(&ue, loc, sd, elements);
        ue.exp().type = type;
    }
    else
    {
        if (type != Type.terror)
        {
            // have to change to internal compiler error
            // all invalid casts should be handled already in Expression::castTo().
            error(loc, "cannot cast `%s` to `%s`", e1.type.toChars(), type.toChars());
        }
        emplaceExp!(ErrorExp)(&ue);
    }
    return ue;
}

UnionExp ArrayLength(Type type, Expression e1)
{
    UnionExp ue = void;
    Loc loc = e1.loc;
    if (StringExp es1 = e1.isStringExp())
    {
        emplaceExp!(IntegerExp)(&ue, loc, es1.len, type);
    }
    else if (ArrayLiteralExp ale = e1.isArrayLiteralExp())
    {
        size_t dim = ale.elements ? ale.elements.length : 0;
        emplaceExp!(IntegerExp)(&ue, loc, dim, type);
    }
    else if (AssocArrayLiteralExp ale = e1.isAssocArrayLiteralExp)
    {
        size_t dim = ale.keys.length;
        emplaceExp!(IntegerExp)(&ue, loc, dim, type);
    }
    else if (e1.type.toBasetype().ty == Tsarray)
    {
        Expression e = (cast(TypeSArray)e1.type.toBasetype()).dim;
        emplaceExp!(UnionExp)(&ue, e);
    }
    else if (e1.isNullExp())
    {
        emplaceExp!(IntegerExp)(&ue, loc, 0, type);
    }
    else
        cantExp(ue);
    return ue;
}

/* Also return EXP.cantExpression if this fails
 */
UnionExp Index(Type type, Expression e1, Expression e2, bool indexIsInBounds)
{
    UnionExp ue = void;
    Loc loc = e1.loc;
    //printf("Index(e1 = %s, e2 = %s)\n", e1.toChars(), e2.toChars());
    assert(e1.type);
    if (e1.op == EXP.string_ && e2.op == EXP.int64)
    {
        StringExp es1 = e1.isStringExp();
        uinteger_t i = e2.toInteger();
        if (i >= es1.len)
        {
            error(e1.loc, "string index %llu is out of bounds `[0 .. %llu]`", i, cast(ulong)es1.len);
            emplaceExp!(ErrorExp)(&ue);
        }
        else
        {
            emplaceExp!(IntegerExp)(&ue, loc, es1.getIndex(cast(size_t) i), type);
        }
    }
    else if (e1.type.toBasetype().ty == Tsarray && e2.op == EXP.int64)
    {
        TypeSArray tsa = cast(TypeSArray)e1.type.toBasetype();
        uinteger_t length = tsa.dim.toInteger();
        uinteger_t i = e2.toInteger();
        if (i >= length && (e1.op == EXP.arrayLiteral || !indexIsInBounds))
        {
            // C code only checks bounds if an ArrayLiteralExp
            error(e1.loc, "array index %llu is out of bounds `%s[0 .. %llu]`", i, e1.toChars(), length);
            emplaceExp!(ErrorExp)(&ue);
        }
        else if (ArrayLiteralExp ale = e1.isArrayLiteralExp())
        {
            auto e = ale[cast(size_t)i];
            e.type = type;
            e.loc = loc;
            if (hasSideEffect(e))
                cantExp(ue);
            else
                emplaceExp!(UnionExp)(&ue, e);
        }
        else
            cantExp(ue);
    }
    else if (e1.type.toBasetype().ty == Tarray && e2.op == EXP.int64)
    {
        uinteger_t i = e2.toInteger();
        if (ArrayLiteralExp ale = e1.isArrayLiteralExp())
        {
            if (i >= ale.elements.length)
            {
                error(e1.loc, "array index %llu is out of bounds `%s[0 .. %llu]`", i, e1.toChars(), cast(ulong) ale.elements.length);
                emplaceExp!(ErrorExp)(&ue);
            }
            else
            {
                auto e = ale[cast(size_t)i];
                e.type = type;
                e.loc = loc;
                if (hasSideEffect(e))
                    cantExp(ue);
                else
                    emplaceExp!(UnionExp)(&ue, e);
            }
        }
        else
            cantExp(ue);
    }
    else if (AssocArrayLiteralExp ae = e1.isAssocArrayLiteralExp())
    {
        /* Search the keys backwards, in case there are duplicate keys
         */
        for (size_t i = ae.keys.length; i;)
        {
            i--;
            Expression ekey = (*ae.keys)[i];
            ue = Equal(EXP.equal, loc, Type.tbool, ekey, e2);
            if (CTFEExp.isCantExp(ue.exp()))
                return ue;
            if (ue.exp().toBool().hasValue(true))
            {
                Expression e = (*ae.values)[i];
                e.type = type;
                e.loc = loc;
                if (hasSideEffect(e))
                    cantExp(ue);
                else
                    emplaceExp!(UnionExp)(&ue, e);
                return ue;
            }
        }
        cantExp(ue);
    }
    else
        cantExp(ue);
    return ue;
}

/* Also return EXP.cantExpression if this fails
 */
UnionExp Slice(Type type, Expression e1, Expression lwr, Expression upr)
{
    UnionExp ue = void;
    Loc loc = e1.loc;
    static if (LOG)
    {
        printf("Slice()\n");
        if (lwr)
        {
            printf("\te1 = %s\n", e1.toChars());
            printf("\tlwr = %s\n", lwr.toChars());
            printf("\tupr = %s\n", upr.toChars());
        }
    }

    if (!lwr)
    {
        if (e1.op == EXP.string_)
            emplaceExp(&ue, e1);
        else
            cantExp(ue);
    }
    else if (e1.op == EXP.string_ && lwr.op == EXP.int64 && upr.op == EXP.int64)
    {
        StringExp es1 = e1.isStringExp();
        const uinteger_t ilwr = lwr.toInteger();
        const uinteger_t iupr = upr.toInteger();
        if (sliceBoundsCheck(0, es1.len, ilwr, iupr))
            cantExp(ue);   // https://issues.dlang.org/show_bug.cgi?id=18115
        else
        {
            const len = cast(size_t)(iupr - ilwr);
            const sz = es1.sz;
            void* s = mem.xmalloc(len * sz);
            const data1 = es1.peekData();
            memcpy(s, data1.ptr + ilwr * sz, len * sz);
            emplaceExp!(StringExp)(&ue, loc, s[0 .. len * sz], len, sz, es1.postfix);
            StringExp es = ue.exp().isStringExp();
            es.committed = es1.committed;
            es.type = type;
        }
    }
    else if (e1.op == EXP.arrayLiteral && lwr.op == EXP.int64 && upr.op == EXP.int64 && !hasSideEffect(e1))
    {
        ArrayLiteralExp es1 = e1.isArrayLiteralExp();
        const uinteger_t ilwr = lwr.toInteger();
        const uinteger_t iupr = upr.toInteger();
        if (sliceBoundsCheck(0, es1.elements.length, ilwr, iupr))
            cantExp(ue);
        else
        {
            auto elements = new Expressions(cast(size_t)(iupr - ilwr));
            memcpy(elements.tdata(), es1.elements.tdata() + ilwr, cast(size_t)(iupr - ilwr) * ((*es1.elements)[0]).sizeof);
            emplaceExp!(ArrayLiteralExp)(&ue, e1.loc, type, elements);
        }
    }
    else
        cantExp(ue);
    return ue;
}

/* Check whether slice `[newlwr .. newupr]` is in the range `[lwr .. upr]`
 */
bool sliceBoundsCheck(uinteger_t lwr, uinteger_t upr, uinteger_t newlwr, uinteger_t newupr) pure @safe
{
    assert(lwr <= upr);
    return !(newlwr <= newupr &&
             lwr <= newlwr &&
             newupr <= upr);
}

/* Set a slice of char/integer array literal 'existingAE' from a string 'newval'.
 * existingAE[firstIndex..firstIndex+newval.length] = newval.
 */
void sliceAssignArrayLiteralFromString(ArrayLiteralExp existingAE, const StringExp newval, size_t firstIndex)
{
    const len = newval.len;
    Type elemType = existingAE.type.nextOf();
    foreach (j; 0 .. len)
    {
        const val = newval.getIndex(j);
        (*existingAE.elements)[j + firstIndex] = new IntegerExp(newval.loc, val, elemType);
    }
}

/* Set a slice of string 'existingSE' from a char array literal 'newae'.
 *   existingSE[firstIndex..firstIndex+newae.length] = newae.
 */
void sliceAssignStringFromArrayLiteral(StringExp existingSE, ArrayLiteralExp newae, size_t firstIndex)
{
    assert(existingSE.ownedByCtfe != OwnedBy.code);
    foreach (j; 0 .. newae.elements.length)
    {
        existingSE.setCodeUnit(firstIndex + j, cast(dchar)newae[j].toInteger());
    }
}

/* Set a slice of string 'existingSE' from a string 'newstr'.
 *   existingSE[firstIndex..firstIndex+newstr.length] = newstr.
 */
void sliceAssignStringFromString(StringExp existingSE, const StringExp newstr, size_t firstIndex)
{
    assert(existingSE.ownedByCtfe != OwnedBy.code);
    size_t sz = existingSE.sz;
    assert(sz == newstr.sz);
    auto data1 = existingSE.borrowData();
    const data2 = newstr.peekData();
    memcpy(data1.ptr + firstIndex * sz, data2.ptr, data2.length);
}

/* Compare a string slice with another string slice.
 * Conceptually equivalent to memcmp( se1[lo1..lo1+len],  se2[lo2..lo2+len])
 */
int sliceCmpStringWithString(const StringExp se1, const StringExp se2, size_t lo1, size_t lo2, size_t len)
{
    size_t sz = se1.sz;
    assert(sz == se2.sz);
    const data1 = se1.peekData();
    const data2 = se2.peekData();
    return memcmp(data1.ptr + sz * lo1, data2.ptr + sz * lo2, sz * len);
}

/* Compare a string slice with an array literal slice
 * Conceptually equivalent to memcmp( se1[lo1..lo1+len],  ae2[lo2..lo2+len])
 */
int sliceCmpStringWithArray(const StringExp se1, ArrayLiteralExp ae2, size_t lo1, size_t lo2, size_t len)
{
    foreach (j; 0 .. len)
    {
        const val2 = ae2[j + lo2].toInteger();
        const val1 = se1.getIndex(j + lo1);
        const int c = (val1 > val2) - (val1 < val2);
        if (c)
            return c;
    }
    return 0;
}

/** Copy element `Expressions` in the parameters when they're `ArrayLiteralExp`s.
 * Params:
 *      e1  = If it's ArrayLiteralExp, its `elements` will be copied.
 *            Otherwise, `e1` itself will be pushed into the new `Expressions`.
 *      e2  = If it's not `null`, it will be pushed/appended to the new
 *            `Expressions` by the same way with `e1`.
 * Returns:
 *      Newly allocated `Expressions`. Note that it points to the original
 *      `Expression` values in e1 and e2.
 */
private Expressions* copyElements(Expression e1, Expression e2 = null)
{
    auto elems = new Expressions();

    void append(ArrayLiteralExp ale)
    {
        if (!ale.elements)
            return;
        auto d = elems.length;
        elems.append(ale.elements);
        foreach (ref el; (*elems)[d .. elems.length])
        {
            if (!el)
                el = ale.basis;
        }
    }

    if (auto ale = e1.isArrayLiteralExp())
        append(ale);
    else
        elems.push(e1);

    if (e2)
    {
        if (auto ale = e2.isArrayLiteralExp())
            append(ale);
        else
            elems.push(e2);
    }

    return elems;
}

/* Also return EXP.cantExpression if this fails
 */
UnionExp Cat(const ref Loc loc, Type type, Expression e1, Expression e2)
{
    UnionExp ue = void;
    Expression e = CTFEExp.cantexp;
    Type t;
    Type t1 = e1.type.toBasetype();
    Type t2 = e2.type.toBasetype();
    //printf("Cat(e1 = %s, e2 = %s)\n", e1.toChars(), e2.toChars());
    //printf("\tt1 = %s, t2 = %s, type = %s\n", t1.toChars(), t2.toChars(), type.toChars());

    /* e is the non-null operand, t is the type of the null operand
     */
    UnionExp catNull(Expression e, Type t)
    {
        Type tn = e.type.toBasetype();
        if (tn.ty.isSomeChar)
        {
            // Create a StringExp
            if (t.nextOf())
                t = t.nextOf().toBasetype();
            const sz = cast(ubyte)t.size();
            dinteger_t v = e.toInteger();
            const len = (t.ty == tn.ty) ? 1 : utf_codeLength(sz, cast(dchar)v);
            void* s = mem.xmalloc(len * sz);
            if (t.ty == tn.ty)
                Port.valcpy(s, v, sz);
            else
                utf_encode(sz, s, cast(dchar)v);
            emplaceExp!(StringExp)(&ue, loc, s[0 .. len * sz], len, sz);
            StringExp es = ue.exp().isStringExp();
            es.type = type;
            es.committed = true;
        }
        else
        {
            // Create an ArrayLiteralExp
            auto elements = new Expressions();
            elements.push(e);
            emplaceExp!(ArrayLiteralExp)(&ue, e.loc, type, elements);
        }
        assert(ue.exp().type);
        return ue;
    }

    if (e1.op == EXP.null_ && (e2.op == EXP.int64 || e2.op == EXP.structLiteral))
    {
        return catNull(e2, t1);
    }
    else if ((e1.op == EXP.int64 || e1.op == EXP.structLiteral) && e2.op == EXP.null_)
    {
        return catNull(e1, t2);
    }
    else if (e1.op == EXP.null_ && e2.op == EXP.null_)
    {
        if (type == e1.type)
        {
            // Handle null ~= null
            if (t1.ty == Tarray && t2 == t1.nextOf())
            {
                emplaceExp!(ArrayLiteralExp)(&ue, e1.loc, type, e2);
                assert(ue.exp().type);
                return ue;
            }
            else
            {
                emplaceExp!(UnionExp)(&ue, e1);
                assert(ue.exp().type);
                return ue;
            }
        }
        if (type == e2.type)
        {
            emplaceExp!(UnionExp)(&ue, e2);
            assert(ue.exp().type);
            return ue;
        }
        emplaceExp!(NullExp)(&ue, e1.loc, type);
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.string_ && e2.op == EXP.string_)
    {
        // Concatenate the strings
        StringExp es1 = e1.isStringExp();
        StringExp es2 = e2.isStringExp();
        size_t len = es1.len + es2.len;
        ubyte sz = es1.sz;
        if (sz != es2.sz)
        {
            /* Can happen with:
             *   auto s = "foo"d ~ "bar"c;
             */
            assert(global.errors);
            cantExp(ue);
            assert(ue.exp().type);
            return ue;
        }
        void* s = mem.xmalloc(len * sz);
        const data1 = es1.peekData();
        const data2 = es2.peekData();
        memcpy(cast(char*)s, data1.ptr, es1.len * sz);
        memcpy(cast(char*)s + es1.len * sz, data2.ptr, es2.len * sz);
        emplaceExp!(StringExp)(&ue, loc, s[0 .. len * sz], len, sz);
        StringExp es = ue.exp().isStringExp();
        es.committed = es1.committed | es2.committed;
        es.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if (e2.op == EXP.string_ && e1.op == EXP.arrayLiteral && t1.nextOf().isintegral())
    {
        // [chars] ~ string --> [chars]
        StringExp es = e2.isStringExp();
        ArrayLiteralExp ea = e1.isArrayLiteralExp();
        size_t len = es.len + ea.elements.length;
        auto elems = new Expressions(len);
        for (size_t i = 0; i < ea.elements.length; ++i)
        {
            (*elems)[i] = ea[i];
        }
        emplaceExp!(ArrayLiteralExp)(&ue, e1.loc, type, elems);
        ArrayLiteralExp dest = ue.exp().isArrayLiteralExp();
        sliceAssignArrayLiteralFromString(dest, es, ea.elements.length);
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.string_ && e2.op == EXP.arrayLiteral && t2.nextOf().isintegral())
    {
        // string ~ [chars] --> [chars]
        StringExp es = e1.isStringExp();
        ArrayLiteralExp ea = e2.isArrayLiteralExp();
        size_t len = es.len + ea.elements.length;
        auto elems = new Expressions(len);
        for (size_t i = 0; i < ea.elements.length; ++i)
        {
            (*elems)[es.len + i] = ea[i];
        }
        emplaceExp!(ArrayLiteralExp)(&ue, e1.loc, type, elems);
        ArrayLiteralExp dest = ue.exp().isArrayLiteralExp();
        sliceAssignArrayLiteralFromString(dest, es, 0);
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.string_ && e2.op == EXP.int64)
    {
        // string ~ char --> string
        StringExp es1 = e1.isStringExp();
        StringExp es;
        const sz = es1.sz;
        dinteger_t v = e2.toInteger();
        // Is it a concatenation of homogenous types?
        // (char[] ~ char, wchar[]~wchar, or dchar[]~dchar)
        bool homoConcat = (sz == t2.size());
        const len = es1.len + (homoConcat ? 1 : utf_codeLength(sz, cast(dchar)v));
        void* s = mem.xmalloc(len * sz);
        const data1 = es1.peekData();
        memcpy(s, data1.ptr, data1.length);
        if (homoConcat)
            Port.valcpy(cast(char*)s + (sz * es1.len), v, sz);
        else
            utf_encode(sz, cast(char*)s + (sz * es1.len), cast(dchar)v);
        emplaceExp!(StringExp)(&ue, loc, s[0 .. len * sz], len, sz);
        es = ue.exp().isStringExp();
        es.committed = es1.committed;
        es.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.int64 && e2.op == EXP.string_)
    {
        // [w|d]?char ~ string --> string
        // We assume that we only ever prepend one char of the same type
        // (wchar,dchar) as the string's characters.
        StringExp es2 = e2.isStringExp();
        const len = 1 + es2.len;
        const sz = es2.sz;
        dinteger_t v = e1.toInteger();
        void* s = mem.xmalloc(len * sz);
        Port.valcpy(cast(char*)s, v, sz);
        const data2 = es2.peekData();
        memcpy(cast(char*)s + sz, data2.ptr, data2.length);
        emplaceExp!(StringExp)(&ue, loc, s[0 .. len * sz], len, sz);
        StringExp es = ue.exp().isStringExp();
        es.sz = sz;
        es.committed = es2.committed;
        es.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.arrayLiteral && e2.op == EXP.arrayLiteral && t1.nextOf().equals(t2.nextOf()))
    {
        // Concatenate the arrays
        auto elems = copyElements(e1, e2);

        emplaceExp!(ArrayLiteralExp)(&ue, e1.loc, cast(Type)null, elems);

        e = ue.exp();
        if (type.toBasetype().ty == Tsarray)
        {
            e.type = t1.nextOf().sarrayOf(elems.length);
        }
        else
            e.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.arrayLiteral && e2.op == EXP.null_ && t1.nextOf().equals(t2.nextOf()))
    {
        e = e1;
        goto L3;
    }
    else if (e1.op == EXP.null_ && e2.op == EXP.arrayLiteral && t1.nextOf().equals(t2.nextOf()))
    {
        e = e2;
    L3:
        // Concatenate the array with null
        auto elems = copyElements(e);

        emplaceExp!(ArrayLiteralExp)(&ue, e.loc, cast(Type)null, elems);

        e = ue.exp();
        if (type.toBasetype().ty == Tsarray)
        {
            e.type = t1.nextOf().sarrayOf(elems.length);
        }
        else
            e.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if ((e1.op == EXP.arrayLiteral || e1.op == EXP.null_) && e1.type.toBasetype().nextOf() && e1.type.toBasetype().nextOf().equals(e2.type))
    {
        auto elems = (e1.op == EXP.arrayLiteral)
                ? copyElements(e1) : new Expressions();
        elems.push(e2);

        emplaceExp!(ArrayLiteralExp)(&ue, loc, cast(Type)null, elems);

        e = ue.exp();
        if (type.toBasetype().ty == Tsarray)
        {
            e.type = e2.type.sarrayOf(elems.length);
        }
        else
            e.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if (e2.op == EXP.arrayLiteral && e2.type.toBasetype().nextOf().equals(e1.type))
    {
        auto elems = copyElements(e1, e2);

        emplaceExp!(ArrayLiteralExp)(&ue, loc, cast(Type)null, elems);

        e = ue.exp();
        if (type.toBasetype().ty == Tsarray)
        {
            e.type = e1.type.sarrayOf(elems.length);
        }
        else
            e.type = type;
        assert(ue.exp().type);
        return ue;
    }
    else if (e1.op == EXP.null_ && e2.op == EXP.string_)
    {
        t = e1.type;
        e = e2;
        goto L1;
    }
    else if (e1.op == EXP.string_ && e2.op == EXP.null_)
    {
        e = e1;
        t = e2.type;
    L1:
        Type tb = t.toBasetype();
        if (tb.ty == Tarray && tb.nextOf().equivalent(e.type))
        {
            auto expressions = new Expressions();
            expressions.push(e);
            emplaceExp!(ArrayLiteralExp)(&ue, loc, t, expressions);
            e = ue.exp();
        }
        else
        {
            emplaceExp!(UnionExp)(&ue, e);
            e = ue.exp();
        }
        if (!e.type.equals(type))
        {
            StringExp se = e.copy().isStringExp();
            e = se.castTo(null, type);
            emplaceExp!(UnionExp)(&ue, e);
            e = ue.exp();
        }
    }
    else
        cantExp(ue);
    assert(ue.exp().type);
    return ue;
}

UnionExp Ptr(Type type, Expression e1)
{
    //printf("Ptr(e1 = %s)\n", e1.toChars());
    UnionExp ue = void;
    if (AddExp ae = e1.isAddExp())
    {
        if (AddrExp ade = ae.e1.isAddrExp())
        {
            if (ae.e2.op == EXP.int64)
                if (StructLiteralExp se = ade.e1.isStructLiteralExp())
                {
                    uint offset = cast(uint)ae.e2.toInteger();
                    Expression e = se.getField(type, offset);
                    if (e)
                    {
                        emplaceExp!(UnionExp)(&ue, e);
                        return ue;
                    }
                }
        }
    }
    cantExp(ue);
    return ue;
}
