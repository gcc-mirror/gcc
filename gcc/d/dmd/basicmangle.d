/**
 * Defines the building blocks for creating the mangled names for basic types.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/basicmangle.d, _basicmangle.d)
 * Documentation:  https://dlang.org/phobos/dmd_basicmangle.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/basicmangle.d
 */
module dmd.basicmangle;

import dmd.astenums;
import dmd.common.outbuffer : OutBuffer;

/// Type mangling mapping for basic, derived and user defined types
immutable char[TMAX] mangleChar =
[
    Tchar        : 'a',
    Tbool        : 'b',
    Tcomplex80   : 'c',
    Tfloat64     : 'd',
    Tfloat80     : 'e',
    Tfloat32     : 'f',
    Tint8        : 'g',
    Tuns8        : 'h',
    Tint32       : 'i',
    Timaginary80 : 'j',
    Tuns32       : 'k',
    Tint64       : 'l',
    Tuns64       : 'm',
    Tnull        : 'n',
    Timaginary32 : 'o',
    Timaginary64 : 'p',
    Tcomplex32   : 'q',
    Tcomplex64   : 'r',
    Tint16       : 's',
    Tuns16       : 't',
    Twchar       : 'u',
    Tvoid        : 'v',
    Tdchar       : 'w',
    //              x   // const
    //              y   // immutable
    Tint128      : 'z', // zi
    Tuns128      : 'z', // zk

    Tarray       : 'A',
    Ttuple       : 'B',
    Tclass       : 'C',
    Tdelegate    : 'D',
    Tenum        : 'E',
    Tfunction    : 'F', // D function
    Tsarray      : 'G',
    Taarray      : 'H',
    //              I   // in
    //              J   // out
    //              K   // ref
    //              L   // lazy
    //              M   // has this, or scope
    //              N   // Nh:vector Ng:wild Nn:noreturn
    //              O   // shared
    Tpointer     : 'P',
    //              Q   // Type/symbol/identifier backward reference
    Treference   : 'R',
    Tstruct      : 'S',
    //              T   // Ttypedef
    //              U   // C function
    //              W   // Windows function
    //              X   // variadic T t...)
    //              Y   // variadic T t,...)
    //              Z   // not variadic, end of parameters

    // '@' shouldn't appear anywhere in the deco'd names
    Tnone        : '@',
    Tident       : '@',
    Tinstance    : '@',
    Terror       : '@',
    Ttypeof      : '@',
    Tslice       : '@',
    Treturn      : '@',
    Tvector      : '@',
    Ttraits      : '@',
    Tmixin       : '@',
    Ttag         : '@',
    Tnoreturn    : '@',         // becomes 'Nn'
];

unittest
{
    foreach (i, mangle; mangleChar)
    {
        if (mangle == char.init)
        {
            import core.stdc.stdio;
            fprintf(stderr, "ty = %u\n", cast(uint)i);
            assert(0);
        }
    }
}

/***********************
 * Mangle basic type ty to buf.
 */
void tyToDecoBuffer(ref OutBuffer buf, int ty) @safe
{
    const c = mangleChar[ty];
    buf.writeByte(c);
    if (c == 'z')
        buf.writeByte(ty == Tint128 ? 'i' : 'k');
}
