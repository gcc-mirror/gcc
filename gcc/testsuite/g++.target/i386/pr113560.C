/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Ofast -std=c++23 -march=znver4" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <immintrin.h>
auto f(char *buf, unsigned long long in) noexcept
{
    unsigned long long hi{};
    auto lo{_mulx_u64(in, 0x2af31dc462ull, &hi)};
    lo = _mulx_u64(lo, 100, &hi);
    __builtin_memcpy(buf + 2, &hi, 2);
    return buf + 10;
}

/* { dg-final { scan-assembler-times "mulx" 1 } } */
/* { dg-final { scan-assembler-times "mulq" 1 } } */
/* { dg-final { scan-assembler-not "addq" } } */
/* { dg-final { scan-assembler-not "adcq" } } */
/* { dg-final { scan-assembler-not "salq" } } */
/* { dg-final { scan-assembler-not "shldq" } } */
