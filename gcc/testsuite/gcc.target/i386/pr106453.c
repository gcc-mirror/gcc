/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mcrc32 -dp" } */
/* { dg-final { scan-assembler-not "zero_extendsidi" } } */

#include <immintrin.h>
#include <stdint.h>

uint32_t f(uint32_t c, uint64_t *p, size_t n)
{
    for (size_t i = 0; i < n; i++)
        c = _mm_crc32_u64(c, p[i]);
    return c;
}
