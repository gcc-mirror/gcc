/**
 * Provides an implicit conversion table for basic types.
 *
 * Used to determine integer promotions and common types.
 *
 * Specification: $(LINK2 https://dlang.org/spec/type.html#integer-promotions, Integer Promotions),
 *   $(LINK2 https://dlang.org/spec/type.html#usual-arithmetic-conversions, Usual Arithmetic Conversions).
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/impcnvtab.d, _impcnvtab.d)
 * Documentation:  https://dlang.org/phobos/dmd_impcnvtab.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/impcnvtab.d
 */

module dmd.impcnvtab;

import dmd.astenums;
import dmd.mtype;

pure @nogc nothrow @safe:

/*************************************************
 * If ty1 and ty2 are basic types, return the TY that both can
 * be implicitly converted to.
 * Params:
 *      ty1 = first operand type
 *      ty2 = second operand type
 * Returns:
 *      ty = common type, else Terror
 */
TY implicitConvCommonTy(TY ty1, TY ty2)
{
    return impCnvTab.impcnvResultTab[ty1][ty2];
}

/*************************************************
 * If ty1 and ty2 are basic types, return the TY that ty1 can
 * be implicitly converted to to bring them to a common ty.
 * It's symmetric, i.e. the operands can be swapped.
 * Params:
 *      ty1 = first operand type
 *      ty2 = second operand type
 * Returns:
 *      ty = what ty1 should be converted to, else Terror
 */
TY implicitConvTy1(TY ty1, TY ty2)
{
    return impCnvTab.impcnvType1Tab[ty1][ty2];
}

/******************************************************************************/

private:

struct ImpCnvTab
{
    TY[TMAX][TMAX] impcnvResultTab;
    TY[TMAX][TMAX] impcnvType1Tab;
}

enum ImpCnvTab impCnvTab = generateImpCnvTab();

ImpCnvTab generateImpCnvTab()
{
    TY[TMAX] typeTYs =
    [
        Tarray,
        Tsarray,
        Taarray,
        Tpointer,
        Treference,
        Tfunction,
        Tident,
        Tclass,
        Tstruct,
        Tenum,
        Tdelegate,
        Tnone,
        Tvoid,
        Tint8,
        Tuns8,
        Tint16,
        Tuns16,
        Tint32,
        Tuns32,
        Tint64,
        Tuns64,
        Tfloat32,
        Tfloat64,
        Tfloat80,
        Timaginary32,
        Timaginary64,
        Timaginary80,
        Tcomplex32,
        Tcomplex64,
        Tcomplex80,
        Tbool,
        Tchar,
        Twchar,
        Tdchar,
        Terror,
        Tinstance,
        Ttypeof,
        Ttuple,
        Tslice,
        Treturn,
        Tnull,
        Tvector,
        Tint128,
        Tuns128,
        Ttraits,
        Tmixin,
        Tnoreturn,
        Ttag,
    ];
    ImpCnvTab impCnvTab;

    // Set conversion tables
    foreach (i; 0 .. cast(size_t)TMAX)
    {
        foreach (j; 0 .. cast(size_t)TMAX)
        {
            impCnvTab.impcnvResultTab[i][j] = Terror;
            impCnvTab.impcnvType1Tab[i][j] = Terror;
        }
    }

    void X(TY t1, TY t2, TY nt1, TY nt2, TY rt)
    {
        impCnvTab.impcnvResultTab[t1][t2] = rt;
        impCnvTab.impcnvResultTab[t2][t1] = rt;

        impCnvTab.impcnvType1Tab[t1][t2] = nt1;
        impCnvTab.impcnvType1Tab[t2][t1] = nt2;
    }

    /* ======================= */

    X(Tbool,Tbool,   Tbool,Tbool,    Tbool);
    X(Tbool,Tint8,   Tint32,Tint32,  Tint32);
    X(Tbool,Tuns8,   Tint32,Tint32,  Tint32);
    X(Tbool,Tint16,  Tint32,Tint32,  Tint32);
    X(Tbool,Tuns16,  Tint32,Tint32,  Tint32);
    X(Tbool,Tint32,  Tint32,Tint32,  Tint32);
    X(Tbool,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tbool,Tint64,  Tint64,Tint64,  Tint64);
    X(Tbool,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tbool,Tint128, Tint128,Tint128, Tint128);
    X(Tbool,Tuns128, Tuns128,Tuns128, Tuns128);

    X(Tbool,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tbool,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tbool,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tbool,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tbool,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tbool,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tbool,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tbool,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tbool,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tint8,Tint8,   Tint32,Tint32,  Tint32);
    X(Tint8,Tuns8,   Tint32,Tint32,  Tint32);
    X(Tint8,Tint16,  Tint32,Tint32,  Tint32);
    X(Tint8,Tuns16,  Tint32,Tint32,  Tint32);
    X(Tint8,Tint32,  Tint32,Tint32,  Tint32);
    X(Tint8,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tint8,Tint64,  Tint64,Tint64,  Tint64);
    X(Tint8,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tint8,Tint128, Tint128,Tint128, Tint128);
    X(Tint8,Tuns128, Tuns128,Tuns128, Tuns128);

    X(Tint8,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tint8,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tint8,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tint8,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tint8,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tint8,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tint8,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tint8,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tint8,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tuns8,Tuns8,   Tint32,Tint32,  Tint32);
    X(Tuns8,Tint16,  Tint32,Tint32,  Tint32);
    X(Tuns8,Tuns16,  Tint32,Tint32,  Tint32);
    X(Tuns8,Tint32,  Tint32,Tint32,  Tint32);
    X(Tuns8,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tuns8,Tint64,  Tint64,Tint64,  Tint64);
    X(Tuns8,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tuns8,Tint128,  Tint128,Tint128,  Tint128);
    X(Tuns8,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tuns8,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tuns8,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tuns8,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tuns8,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tuns8,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tuns8,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tuns8,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tuns8,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tuns8,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tint16,Tint16,  Tint32,Tint32,  Tint32);
    X(Tint16,Tuns16,  Tint32,Tint32,  Tint32);
    X(Tint16,Tint32,  Tint32,Tint32,  Tint32);
    X(Tint16,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tint16,Tint64,  Tint64,Tint64,  Tint64);
    X(Tint16,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tint16,Tint128,  Tint128,Tint128,  Tint128);
    X(Tint16,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tint16,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tint16,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tint16,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tint16,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tint16,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tint16,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tint16,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tint16,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tint16,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tuns16,Tuns16,  Tint32,Tint32,  Tint32);
    X(Tuns16,Tint32,  Tint32,Tint32,  Tint32);
    X(Tuns16,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tuns16,Tint64,  Tint64,Tint64,  Tint64);
    X(Tuns16,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tuns16,Tint128, Tint128,Tint128,  Tint128);
    X(Tuns16,Tuns128, Tuns128,Tuns128,  Tuns128);

    X(Tuns16,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tuns16,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tuns16,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tuns16,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tuns16,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tuns16,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tuns16,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tuns16,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tuns16,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tint32,Tint32,  Tint32,Tint32,  Tint32);
    X(Tint32,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tint32,Tint64,  Tint64,Tint64,  Tint64);
    X(Tint32,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tint32,Tint128, Tint128,Tint128,  Tint128);
    X(Tint32,Tuns128, Tuns128,Tuns128,  Tuns128);

    X(Tint32,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tint32,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tint32,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tint32,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tint32,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tint32,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tint32,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tint32,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tint32,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tuns32,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(Tuns32,Tint64,  Tint64,Tint64,  Tint64);
    X(Tuns32,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tuns32,Tint128,  Tint128,Tint128,  Tint128);
    X(Tuns32,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tuns32,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tuns32,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tuns32,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tuns32,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tuns32,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tuns32,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tuns32,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tuns32,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tuns32,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tint64,Tint64,  Tint64,Tint64,  Tint64);
    X(Tint64,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tint64,Tint128,  Tint128,Tint128,  Tint128);
    X(Tint64,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tint64,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tint64,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tint64,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tint64,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tint64,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tint64,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tint64,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tint64,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tint64,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tuns64,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(Tuns64,Tint128,  Tint128,Tint128,  Tint128);
    X(Tuns64,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tuns64,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tuns64,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tuns64,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tuns64,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tuns64,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tuns64,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tuns64,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tuns64,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tuns64,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tint128,Tint128,  Tint128,Tint128,  Tint128);
    X(Tint128,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tint128,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tint128,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tint128,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tint128,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tint128,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tint128,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tint128,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tint128,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tint128,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tuns128,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(Tuns128,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(Tuns128,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(Tuns128,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(Tuns128,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(Tuns128,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(Tuns128,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(Tuns128,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(Tuns128,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(Tuns128,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(Tfloat32,Tfloat32,  Tfloat32,Tfloat32, Tfloat32);
    X(Tfloat32,Tfloat64,  Tfloat64,Tfloat64, Tfloat64);
    X(Tfloat32,Tfloat80,  Tfloat80,Tfloat80, Tfloat80);

    X(Tfloat32,Timaginary32,  Tfloat32,Timaginary32, Tfloat32);
    X(Tfloat32,Timaginary64,  Tfloat64,Timaginary64, Tfloat64);
    X(Tfloat32,Timaginary80,  Tfloat80,Timaginary80, Tfloat80);

    X(Tfloat32,Tcomplex32,  Tfloat32,Tcomplex32, Tcomplex32);
    X(Tfloat32,Tcomplex64,  Tfloat64,Tcomplex64, Tcomplex64);
    X(Tfloat32,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Tfloat64,Tfloat64,  Tfloat64,Tfloat64, Tfloat64);
    X(Tfloat64,Tfloat80,  Tfloat80,Tfloat80, Tfloat80);

    X(Tfloat64,Timaginary32,  Tfloat64,Timaginary64, Tfloat64);
    X(Tfloat64,Timaginary64,  Tfloat64,Timaginary64, Tfloat64);
    X(Tfloat64,Timaginary80,  Tfloat80,Timaginary80, Tfloat80);

    X(Tfloat64,Tcomplex32,  Tfloat64,Tcomplex64, Tcomplex64);
    X(Tfloat64,Tcomplex64,  Tfloat64,Tcomplex64, Tcomplex64);
    X(Tfloat64,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Tfloat80,Tfloat80,  Tfloat80,Tfloat80, Tfloat80);

    X(Tfloat80,Timaginary32,  Tfloat80,Timaginary80, Tfloat80);
    X(Tfloat80,Timaginary64,  Tfloat80,Timaginary80, Tfloat80);
    X(Tfloat80,Timaginary80,  Tfloat80,Timaginary80, Tfloat80);

    X(Tfloat80,Tcomplex32,  Tfloat80,Tcomplex80, Tcomplex80);
    X(Tfloat80,Tcomplex64,  Tfloat80,Tcomplex80, Tcomplex80);
    X(Tfloat80,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Timaginary32,Timaginary32,  Timaginary32,Timaginary32, Timaginary32);
    X(Timaginary32,Timaginary64,  Timaginary64,Timaginary64, Timaginary64);
    X(Timaginary32,Timaginary80,  Timaginary80,Timaginary80, Timaginary80);

    X(Timaginary32,Tcomplex32,  Timaginary32,Tcomplex32, Tcomplex32);
    X(Timaginary32,Tcomplex64,  Timaginary64,Tcomplex64, Tcomplex64);
    X(Timaginary32,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Timaginary64,Timaginary64,  Timaginary64,Timaginary64, Timaginary64);
    X(Timaginary64,Timaginary80,  Timaginary80,Timaginary80, Timaginary80);

    X(Timaginary64,Tcomplex32,  Timaginary64,Tcomplex64, Tcomplex64);
    X(Timaginary64,Tcomplex64,  Timaginary64,Tcomplex64, Tcomplex64);
    X(Timaginary64,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Timaginary80,Timaginary80,  Timaginary80,Timaginary80, Timaginary80);

    X(Timaginary80,Tcomplex32,  Timaginary80,Tcomplex80, Tcomplex80);
    X(Timaginary80,Tcomplex64,  Timaginary80,Tcomplex80, Tcomplex80);
    X(Timaginary80,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Tcomplex32,Tcomplex32,  Tcomplex32,Tcomplex32, Tcomplex32);
    X(Tcomplex32,Tcomplex64,  Tcomplex64,Tcomplex64, Tcomplex64);
    X(Tcomplex32,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Tcomplex64,Tcomplex64,  Tcomplex64,Tcomplex64, Tcomplex64);
    X(Tcomplex64,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(Tcomplex80,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80);

    // "No type is implicitly convertible to noreturn, but noreturn is implicitly convertible to every other type"
    foreach(convertToTy; typeTYs)
        X(Tnoreturn, convertToTy, convertToTy, convertToTy, convertToTy);

    return impCnvTab;
}
