/**
 * Implements a complex number type.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/complex.d, _complex.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_complex.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/complex.d
 */

module dmd.root.complex;

import dmd.root.ctfloat;

nothrow:

extern (C++) struct complex_t
{
    real_t re;
    real_t im;

  nothrow:

    this() @disable;

    this(real_t re)
    {
        this(re, CTFloat.zero);
    }

    this(real_t re, real_t im)
    {
        this.re = re;
        this.im = im;
    }

    extern (D) complex_t opBinary(string op)(complex_t y)
        if (op == "+")
    {
        return complex_t(re + y.re, im + y.im);
    }

    extern (D) complex_t opBinary(string op)(complex_t y)
        if (op == "-")
    {
        return complex_t(re - y.re, im - y.im);
    }

    extern (D) complex_t opUnary(string op)()
        if (op == "-")
    {
        return complex_t(-re, -im);
    }

    extern (D) complex_t opBinary(string op)(complex_t y)
        if (op == "*")
    {
        return complex_t(re * y.re - im * y.im, im * y.re + re * y.im);
    }

    extern (D) complex_t opBinaryRight(string op)(real_t x)
        if (op == "*")
    {
        return complex_t(x) * this;
    }

    extern (D) complex_t opBinary(string op)(real_t y)
        if (op == "*")
    {
        return this * complex_t(y);
    }

    extern (D) complex_t opBinary(string op)(real_t y)
        if (op == "/")
    {
        return this / complex_t(y);
    }

    extern (D) complex_t opBinary(string op)(complex_t y)
        if (op == "/")
    {
        if (CTFloat.fabs(y.re) < CTFloat.fabs(y.im))
        {
            const r = y.re / y.im;
            const den = y.im + r * y.re;
            return complex_t((re * r + im) / den, (im * r - re) / den);
        }
        else
        {
            const r = y.im / y.re;
            const den = y.re + r * y.im;
            return complex_t((re + r * im) / den, (im - r * re) / den);
        }
    }

    extern (D) bool opCast(T : bool)() const
    {
        return re || im;
    }

    int opEquals(complex_t y) const
    {
        return re == y.re && im == y.im;
    }
}

extern (C++) real_t creall(complex_t x)
{
    return x.re;
}

extern (C++) real_t cimagl(complex_t x)
{
    return x.im;
}
