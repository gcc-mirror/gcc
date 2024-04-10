/**
 * Collects functions for compile-time floating-point calculations.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/ctfloat.d, root/_ctfloat.d)
 * Documentation: https://dlang.org/phobos/dmd_root_ctfloat.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/ctfloat.d
 */

module dmd.root.ctfloat;

nothrow:

// Type used by the front-end for compile-time reals
public import dmd.root.longdouble : real_t = longdouble;

// Compile-time floating-point helper
extern (C++) struct CTFloat
{
  nothrow:
  @nogc:
  @safe:

    version (GNU)
        enum yl2x_supported = false;
    else
        enum yl2x_supported = __traits(compiles, core.math.yl2x(1.0L, 2.0L));
    enum yl2xp1_supported = yl2x_supported;

    pure static real_t fabs(real_t x);
    pure static real_t ldexp(real_t n, int exp);

    pure @trusted
    static bool isIdentical(real_t a, real_t b);

    pure @trusted
    static size_t hash(real_t a);

    pure
    static bool isNaN(real_t r);

    pure @trusted
    static bool isSNaN(real_t r);

    static bool isInfinity(real_t r) pure;

    @system
    static real_t parse(const(char)* literal, out bool isOutOfRange);

    @system
    static int sprint(char* str, size_t size, char fmt, real_t x);

    // Constant real values 0, 1, -1 and 0.5.
    __gshared real_t zero;
    __gshared real_t one;
    __gshared real_t minusone;
    __gshared real_t half;

    @trusted
    static void initialize();
}
