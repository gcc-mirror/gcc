/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64-v4 -std=c++17" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

#include <x86intrin.h>

/*
**_Z8qustrchrPDsS_Ds:
**...
**.L[0-9]+:
**	vpcmpeqw	\(%[a-x]+\), %ymm0, %k1
**	vpcmpeqw	32\(%[a-x]+\), %ymm0, %k0
**	kortestw	%k0, %k1
**	je	.L[0-9]+
**...
*/

const char16_t *
qustrchr(char16_t *n, char16_t *e, char16_t c) noexcept
{
    __m256i mch256 = _mm256_set1_epi16(c);
    for ( ; n < e; n += 32) {
        __m256i data1 = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(n));
        __m256i data2 = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(n) + 1);
        __mmask16 mask1 = _mm256_cmpeq_epu16_mask(data1, mch256);
        __mmask16 mask2 = _mm256_cmpeq_epu16_mask(data2, mch256);
        if (_kortestz_mask16_u8(mask1, mask2))
            continue;

        unsigned idx = _tzcnt_u32(mask1);
        if (mask1 == 0) {
            idx = __tzcnt_u16(mask2);
            n += 16;
        }
        return n + idx;
    }
    return e;
}
