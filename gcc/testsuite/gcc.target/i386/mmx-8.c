/* PR middle-end/37809 */

/* { dg-do run } */
/* { dg-options "-O2 -mmmx" } */

#include <mmintrin.h>

#include "mmx-check.h"

// Various tests of cases where it is incorrect to optimise vectors as if they
// were integers of the same width.

extern void abort (void);

void __attribute__ ((noinline))
Sshift()
{
    volatile __m64 y = (__m64) 0xffffffffll;
    __m64 x = y & (__m64) 0xffffffffll;
    x = _m_psradi (x, 1);
    x &= (__m64) 0x80000000ll;
    if (0 == (long long) x)
        abort();
}

#define SHIFTU(F,B,S,T)                         \
    void F()                                    \
    {                                           \
        volatile __m64 y = (__m64) 0ll;         \
        __m64 x = y | (__m64) (1llu << B);      \
        if (S > 0)                              \
            x = _m_pslldi (x, S);               \
        else                                    \
            x = _m_psrldi (x, -S);              \
        if (T > 0)                              \
            x = _m_pslldi (x, T);               \
        else                                    \
            x = _m_psrldi (x, -T);              \
        x &= (__m64) (1llu << (B + S + T));     \
        if ((long long) x)                      \
            abort();                            \
    }

SHIFTU (shiftU1, 31, 1, -1)
SHIFTU (shiftU2, 32, -1, 1)
SHIFTU (shiftU3, 31, 1, 0)
SHIFTU (shiftU4, 32, -1, 0)

void __attribute__ ((noinline))
add_1()
{
    volatile long long ONE = 1;
    long long one = ONE;

    __m64 a = (__m64) one;
    __m64 b = (__m64) -one;
    __m64 c = a + b;
    if (0 == (long long) c)
        abort();
}

void __attribute__ ((noinline))
add_2()
{
    volatile long long ONE = 1;
    long long one = ONE;

    __m64 a = (__m64) one;
    __m64 b = (__m64) -one;
    __m64 c = _m_paddd (a, b);
    if (0 == (long long) c)
        abort();
}

void __attribute__ ((noinline))
mult_1()
{
    volatile __m64 y = (__m64) 0ll;
    __m64 x = y | (__m64) (1ll << 32);
    x = x * (__m64) 1ll;
    x &= (__m64) (1ll << 32);
    if (0 != (long long) x)
        abort();
}

void __attribute__ ((noinline))
mult_2()
{
    volatile int foo = 1;
    unsigned long long one = foo & 1;

    __m64 x = (__m64) (one << 16);
    x *= x;
    x &= (__m64) (1ll << 32);
    if (0 != (long long) x)
        abort();
}

void __attribute__ ((noinline))
mult_3()
{
    volatile __m64 y = (__m64) (1ll << 32);
    __m64 a = y;
    __m64 b = y * (__m64) 1ll;
    if (((long long) a) == (long long) b)
        abort();
}

void __attribute__ ((noinline))
div_1()
{
    volatile __m64 y = (__m64) 0ll;
    __m64 x = y | (__m64) (1ull << 32);
    x |= (__m64) 1ull;
    x = x / x;
    if (1ll == (long long) x)
        abort();
}


void mmx_test (void)
{
    Sshift();
    shiftU1();
    shiftU2();
    shiftU3();
    shiftU4();

    add_1();
    add_2();

    mult_1();
    mult_2();
    mult_3();

    div_1();
}
