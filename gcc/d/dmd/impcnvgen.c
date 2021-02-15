
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/impcnvgen.c
 */

#include "root/dsystem.h"

#include "mtype.h"

TY impcnvResultTab[TMAX][TMAX];
TY impcnvType1Tab[TMAX][TMAX];
TY impcnvType2Tab[TMAX][TMAX];
int impcnvWarnTab[TMAX][TMAX];

int integral_promotion(int t)
{
    switch (t)
    {
        case Tchar:
        case Twchar:
        case Tbool:
        case Tint8:
        case Tuns8:
        case Tint16:
        case Tuns16:    return Tint32;
        case Tdchar:    return Tuns32;
        default:        return t;
    }
}

void init()
{   int i, j;

    // Set conversion tables
    for (i = 0; i < TMAX; i++)
        for (j = 0; j < TMAX; j++)
        {   impcnvResultTab[i][j] = Terror;
            impcnvType1Tab[i][j] = Terror;
            impcnvType2Tab[i][j] = Terror;
            impcnvWarnTab[i][j] = 0;
        }

#define X(t1,t2, nt1,nt2, rt)           \
        impcnvResultTab[t1][t2] = rt;      \
        impcnvType1Tab[t1][t2] = nt1;      \
        impcnvType2Tab[t1][t2] = nt2;


    /* ======================= */

    X(Tbool,Tbool,   Tbool,Tbool,    Tbool)
    X(Tbool,Tint8,   Tint32,Tint32,  Tint32)
    X(Tbool,Tuns8,   Tint32,Tint32,  Tint32)
    X(Tbool,Tint16,  Tint32,Tint32,  Tint32)
    X(Tbool,Tuns16,  Tint32,Tint32,  Tint32)
    X(Tbool,Tint32,  Tint32,Tint32,  Tint32)
    X(Tbool,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tbool,Tint64,  Tint64,Tint64,  Tint64)
    X(Tbool,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tbool,Tint128, Tint128,Tint128, Tint128)
    X(Tbool,Tuns128, Tuns128,Tuns128, Tuns128)

    X(Tbool,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tbool,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tbool,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tbool,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tbool,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tbool,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tbool,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tbool,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tbool,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tint8,Tint8,   Tint32,Tint32,  Tint32)
    X(Tint8,Tuns8,   Tint32,Tint32,  Tint32)
    X(Tint8,Tint16,  Tint32,Tint32,  Tint32)
    X(Tint8,Tuns16,  Tint32,Tint32,  Tint32)
    X(Tint8,Tint32,  Tint32,Tint32,  Tint32)
    X(Tint8,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tint8,Tint64,  Tint64,Tint64,  Tint64)
    X(Tint8,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tint8,Tint128, Tint128,Tint128, Tint128)
    X(Tint8,Tuns128, Tuns128,Tuns128, Tuns128)

    X(Tint8,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tint8,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tint8,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tint8,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tint8,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tint8,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tint8,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tint8,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tint8,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tuns8,Tuns8,   Tint32,Tint32,  Tint32)
    X(Tuns8,Tint16,  Tint32,Tint32,  Tint32)
    X(Tuns8,Tuns16,  Tint32,Tint32,  Tint32)
    X(Tuns8,Tint32,  Tint32,Tint32,  Tint32)
    X(Tuns8,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tuns8,Tint64,  Tint64,Tint64,  Tint64)
    X(Tuns8,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tuns8,Tint128,  Tint128,Tint128,  Tint128)
    X(Tuns8,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tuns8,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tuns8,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tuns8,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tuns8,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tuns8,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tuns8,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tuns8,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tuns8,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tuns8,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tint16,Tint16,  Tint32,Tint32,  Tint32)
    X(Tint16,Tuns16,  Tint32,Tint32,  Tint32)
    X(Tint16,Tint32,  Tint32,Tint32,  Tint32)
    X(Tint16,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tint16,Tint64,  Tint64,Tint64,  Tint64)
    X(Tint16,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tint16,Tint128,  Tint128,Tint128,  Tint128)
    X(Tint16,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tint16,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tint16,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tint16,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tint16,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tint16,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tint16,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tint16,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tint16,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tint16,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tuns16,Tuns16,  Tint32,Tint32,  Tint32)
    X(Tuns16,Tint32,  Tint32,Tint32,  Tint32)
    X(Tuns16,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tuns16,Tint64,  Tint64,Tint64,  Tint64)
    X(Tuns16,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tuns16,Tint128, Tint128,Tint128,  Tint128)
    X(Tuns16,Tuns128, Tuns128,Tuns128,  Tuns128)

    X(Tuns16,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tuns16,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tuns16,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tuns16,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tuns16,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tuns16,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tuns16,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tuns16,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tuns16,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tint32,Tint32,  Tint32,Tint32,  Tint32)
    X(Tint32,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tint32,Tint64,  Tint64,Tint64,  Tint64)
    X(Tint32,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tint32,Tint128, Tint128,Tint128,  Tint128)
    X(Tint32,Tuns128, Tuns128,Tuns128,  Tuns128)

    X(Tint32,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tint32,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tint32,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tint32,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tint32,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tint32,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tint32,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tint32,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tint32,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tuns32,Tuns32,  Tuns32,Tuns32,  Tuns32)
    X(Tuns32,Tint64,  Tint64,Tint64,  Tint64)
    X(Tuns32,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tuns32,Tint128,  Tint128,Tint128,  Tint128)
    X(Tuns32,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tuns32,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tuns32,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tuns32,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tuns32,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tuns32,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tuns32,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tuns32,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tuns32,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tuns32,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tint64,Tint64,  Tint64,Tint64,  Tint64)
    X(Tint64,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tint64,Tint128,  Tint128,Tint128,  Tint128)
    X(Tint64,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tint64,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tint64,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tint64,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tint64,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tint64,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tint64,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tint64,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tint64,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tint64,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tuns64,Tuns64,  Tuns64,Tuns64,  Tuns64)
    X(Tuns64,Tint128,  Tint128,Tint128,  Tint128)
    X(Tuns64,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tuns64,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tuns64,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tuns64,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tuns64,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tuns64,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tuns64,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tuns64,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tuns64,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tuns64,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tint128,Tint128,  Tint128,Tint128,  Tint128)
    X(Tint128,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tint128,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tint128,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tint128,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tint128,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tint128,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tint128,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tint128,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tint128,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tint128,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tuns128,Tuns128,  Tuns128,Tuns128,  Tuns128)

    X(Tuns128,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32)
    X(Tuns128,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64)
    X(Tuns128,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80)
    X(Tuns128,Timaginary32, Tfloat32,Timaginary32, Tfloat32)
    X(Tuns128,Timaginary64, Tfloat64,Timaginary64, Tfloat64)
    X(Tuns128,Timaginary80, Tfloat80,Timaginary80, Tfloat80)
    X(Tuns128,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32)
    X(Tuns128,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64)
    X(Tuns128,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80)

    /* ======================= */

    X(Tfloat32,Tfloat32,  Tfloat32,Tfloat32, Tfloat32)
    X(Tfloat32,Tfloat64,  Tfloat64,Tfloat64, Tfloat64)
    X(Tfloat32,Tfloat80,  Tfloat80,Tfloat80, Tfloat80)

    X(Tfloat32,Timaginary32,  Tfloat32,Timaginary32, Tfloat32)
    X(Tfloat32,Timaginary64,  Tfloat64,Timaginary64, Tfloat64)
    X(Tfloat32,Timaginary80,  Tfloat80,Timaginary80, Tfloat80)

    X(Tfloat32,Tcomplex32,  Tfloat32,Tcomplex32, Tcomplex32)
    X(Tfloat32,Tcomplex64,  Tfloat64,Tcomplex64, Tcomplex64)
    X(Tfloat32,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Tfloat64,Tfloat64,  Tfloat64,Tfloat64, Tfloat64)
    X(Tfloat64,Tfloat80,  Tfloat80,Tfloat80, Tfloat80)

    X(Tfloat64,Timaginary32,  Tfloat64,Timaginary64, Tfloat64)
    X(Tfloat64,Timaginary64,  Tfloat64,Timaginary64, Tfloat64)
    X(Tfloat64,Timaginary80,  Tfloat80,Timaginary80, Tfloat80)

    X(Tfloat64,Tcomplex32,  Tfloat64,Tcomplex64, Tcomplex64)
    X(Tfloat64,Tcomplex64,  Tfloat64,Tcomplex64, Tcomplex64)
    X(Tfloat64,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Tfloat80,Tfloat80,  Tfloat80,Tfloat80, Tfloat80)

    X(Tfloat80,Timaginary32,  Tfloat80,Timaginary80, Tfloat80)
    X(Tfloat80,Timaginary64,  Tfloat80,Timaginary80, Tfloat80)
    X(Tfloat80,Timaginary80,  Tfloat80,Timaginary80, Tfloat80)

    X(Tfloat80,Tcomplex32,  Tfloat80,Tcomplex80, Tcomplex80)
    X(Tfloat80,Tcomplex64,  Tfloat80,Tcomplex80, Tcomplex80)
    X(Tfloat80,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Timaginary32,Timaginary32,  Timaginary32,Timaginary32, Timaginary32)
    X(Timaginary32,Timaginary64,  Timaginary64,Timaginary64, Timaginary64)
    X(Timaginary32,Timaginary80,  Timaginary80,Timaginary80, Timaginary80)

    X(Timaginary32,Tcomplex32,  Timaginary32,Tcomplex32, Tcomplex32)
    X(Timaginary32,Tcomplex64,  Timaginary64,Tcomplex64, Tcomplex64)
    X(Timaginary32,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Timaginary64,Timaginary64,  Timaginary64,Timaginary64, Timaginary64)
    X(Timaginary64,Timaginary80,  Timaginary80,Timaginary80, Timaginary80)

    X(Timaginary64,Tcomplex32,  Timaginary64,Tcomplex64, Tcomplex64)
    X(Timaginary64,Tcomplex64,  Timaginary64,Tcomplex64, Tcomplex64)
    X(Timaginary64,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Timaginary80,Timaginary80,  Timaginary80,Timaginary80, Timaginary80)

    X(Timaginary80,Tcomplex32,  Timaginary80,Tcomplex80, Tcomplex80)
    X(Timaginary80,Tcomplex64,  Timaginary80,Tcomplex80, Tcomplex80)
    X(Timaginary80,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Tcomplex32,Tcomplex32,  Tcomplex32,Tcomplex32, Tcomplex32)
    X(Tcomplex32,Tcomplex64,  Tcomplex64,Tcomplex64, Tcomplex64)
    X(Tcomplex32,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Tcomplex64,Tcomplex64,  Tcomplex64,Tcomplex64, Tcomplex64)
    X(Tcomplex64,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80)

    /* ======================= */

    X(Tcomplex80,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80)

#undef X

#define Y(t1,t2)        impcnvWarnTab[t1][t2] = 1;

    Y(Tuns8, Tint8)
    Y(Tint16, Tint8)
    Y(Tuns16, Tint8)
    Y(Tint32, Tint8)
    Y(Tuns32, Tint8)
    Y(Tint64, Tint8)
    Y(Tuns64, Tint8)
    Y(Tint128, Tint8)
    Y(Tuns128, Tint8)

    Y(Tint8, Tuns8)
    Y(Tint16, Tuns8)
    Y(Tuns16, Tuns8)
    Y(Tint32, Tuns8)
    Y(Tuns32, Tuns8)
    Y(Tint64, Tuns8)
    Y(Tuns64, Tuns8)
    Y(Tint128, Tuns8)
    Y(Tuns128, Tuns8)

    Y(Tint8, Tchar)
    Y(Tint16, Tchar)
    Y(Tuns16, Tchar)
    Y(Tint32, Tchar)
    Y(Tuns32, Tchar)
    Y(Tint64, Tchar)
    Y(Tuns64, Tchar)
    Y(Tint128, Tchar)
    Y(Tuns128, Tchar)

    Y(Tuns16, Tint16)
    Y(Tint32, Tint16)
    Y(Tuns32, Tint16)
    Y(Tint64, Tint16)
    Y(Tuns64, Tint16)
    Y(Tint128, Tint16)
    Y(Tuns128, Tint16)

    Y(Tint16, Tuns16)
    Y(Tint32, Tuns16)
    Y(Tuns32, Tuns16)
    Y(Tint64, Tuns16)
    Y(Tuns64, Tuns16)
    Y(Tint128, Tuns16)
    Y(Tuns128, Tuns16)

    Y(Tint16, Twchar)
    Y(Tint32, Twchar)
    Y(Tuns32, Twchar)
    Y(Tint64, Twchar)
    Y(Tuns64, Twchar)
    Y(Tint128, Twchar)
    Y(Tuns128, Twchar)

//    Y(Tuns32, Tint32)
    Y(Tint64, Tint32)
    Y(Tuns64, Tint32)
    Y(Tint128, Tint32)
    Y(Tuns128, Tint32)

//    Y(Tint32, Tuns32)
    Y(Tint64, Tuns32)
    Y(Tuns64, Tuns32)
    Y(Tint128, Tuns32)
    Y(Tuns128, Tuns32)

    Y(Tint64, Tdchar)
    Y(Tuns64, Tdchar)
    Y(Tint128, Tdchar)
    Y(Tuns128, Tdchar)

//    Y(Tint64, Tuns64)
//    Y(Tuns64, Tint64)
    Y(Tint128, Tint64)
    Y(Tuns128, Tint64)
    Y(Tint128, Tuns64)
    Y(Tuns128, Tuns64)

//    Y(Tint128, Tuns128)
//    Y(Tuns128, Tint128)

    for (i = 0; i < TMAX; i++)
        for (j = 0; j < TMAX; j++)
        {
            if (impcnvResultTab[i][j] == Terror)
            {
                impcnvResultTab[i][j] = impcnvResultTab[j][i];
                impcnvType1Tab[i][j] = impcnvType2Tab[j][i];
                impcnvType2Tab[i][j] = impcnvType1Tab[j][i];
            }
        }
}

int main()
{
    int i;
    int j;

    init();

    {
        FILE *fp = fopen("impcnvtab.c","wb");

        fprintf(fp,"// This file is generated by impcnvgen.c\n");
        fprintf(fp,"#include \"mtype.h\"\n");

        fprintf(fp,"unsigned char impcnvResult[TMAX][TMAX] =\n{\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",");
            fprintf(fp, "{");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d",impcnvResultTab[i][j]);
            }
            fprintf(fp, "}\n");
        }
        fprintf(fp,"};\n");

        fprintf(fp,"unsigned char impcnvType1[TMAX][TMAX] =\n{\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",");
            fprintf(fp, "{");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d",impcnvType1Tab[i][j]);
            }
            fprintf(fp, "}\n");
        }
        fprintf(fp,"};\n");

        fprintf(fp,"unsigned char impcnvType2[TMAX][TMAX] =\n{\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",");
            fprintf(fp, "{");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d",impcnvType2Tab[i][j]);
            }
            fprintf(fp, "}\n");
        }
        fprintf(fp,"};\n");

        fprintf(fp,"unsigned char impcnvWarn[TMAX][TMAX] =\n{\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",");
            fprintf(fp, "{");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d",impcnvWarnTab[i][j]);
            }
            fprintf(fp, "}\n");
        }
        fprintf(fp,"};\n");

        fclose(fp);
    }

    {
        FILE *fp = fopen("impcnvtab.d", "wb");

        fprintf(fp, "// This file is generated by impcnvgen.c\n");
        fprintf(fp, "module ddmd.impcnvtab;\n");
        fprintf(fp, "\n");
        fprintf(fp, "import ddmd.mtype;\n");
        fprintf(fp, "\n");

        fprintf(fp, "extern (C++) __gshared ubyte[TMAX][TMAX] impcnvResult =\n[\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",\n");
            fprintf(fp, "    [");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d", impcnvResultTab[i][j]);
            }
            fprintf(fp, "]");
        }
        fprintf(fp, "\n];\n");

        fprintf(fp, "extern (C++) __gshared ubyte[TMAX][TMAX] impcnvType1 =\n[\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",\n");
            fprintf(fp, "    [");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d", impcnvType1Tab[i][j]);
            }
            fprintf(fp, "]");
        }
        fprintf(fp, "\n];\n");

        fprintf(fp, "extern (C++) __gshared ubyte[TMAX][TMAX] impcnvType2 =\n[\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",\n");
            fprintf(fp, "    [");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d",impcnvType2Tab[i][j]);
            }
            fprintf(fp, "]");
        }
        fprintf(fp,"\n];\n");

        fprintf(fp,"extern (C++) __gshared ubyte[TMAX][TMAX] impcnvWarn =\n[\n");
        for (i = 0; i < TMAX; i++)
        {
            if (i)
                fprintf(fp, ",\n");
            fprintf(fp, "    [");
            for (j = 0; j < TMAX; j++)
            {
                if (j)
                    fprintf(fp, ",");
                fprintf(fp, "%d", impcnvWarnTab[i][j]);
            }
            fprintf(fp, "]");
        }
        fprintf(fp, "\n];\n");

        fclose(fp);
    }

    return EXIT_SUCCESS;
}
