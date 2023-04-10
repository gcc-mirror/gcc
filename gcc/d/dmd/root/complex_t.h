
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/complex_t.h
 */

#pragma once

#include "ctfloat.h"

/* Roll our own complex type for compilers that don't support complex
 */

struct complex_t
{
    real_t re;
    real_t im;

    complex_t(real_t re) : re(re), im(CTFloat::zero) {}
    complex_t(real_t re, real_t im) : re(re), im(im) {}

    complex_t operator + (complex_t y) { return complex_t(re + y.re, im + y.im); }
    complex_t operator - (complex_t y) { return complex_t(re - y.re, im - y.im); }
    complex_t operator - () { return complex_t(-re, -im); }
    complex_t operator * (complex_t y) { return complex_t(re * y.re - im * y.im, im * y.re + re * y.im); }

    complex_t operator / (complex_t y)
    {
        if (CTFloat::fabs(y.re) < CTFloat::fabs(y.im))
        {
            real_t r = y.re / y.im;
            real_t den = y.im + r * y.re;
            return complex_t((re * r + im) / den,
                             (im * r - re) / den);
        }
        else
        {
            real_t r = y.im / y.re;
            real_t den = y.re + r * y.im;
            return complex_t((re + r * im) / den,
                             (im - r * re) / den);
        }
    }

    operator bool () { return re || im; }

    int operator == (complex_t y) { return re == y.re && im == y.im; }
    int operator != (complex_t y) { return re != y.re || im != y.im; }

private:
    complex_t() : re(CTFloat::zero), im(CTFloat::zero) {}
};

inline complex_t operator * (real_t x, complex_t y) { return complex_t(x) * y; }
inline complex_t operator * (complex_t x, real_t y) { return x * complex_t(y); }
inline complex_t operator / (complex_t x, real_t y) { return x / complex_t(y); }


inline real_t creall(complex_t x)
{
    return x.re;
}

inline real_t cimagl(complex_t x)
{
    return x.im;
}
