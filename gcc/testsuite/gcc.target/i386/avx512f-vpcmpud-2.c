/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#include <math.h>
#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#if AVX512F_LEN == 512
#define CMP(imm, rel)					\
    dst_ref = 0;					\
    for (i = 0; i < 16; i++)				\
    {							\
      dst_ref = ((rel) << i) | dst_ref;			\
    }							\
    source1.x = _mm512_loadu_si512 (s1);		\
    source2.x = _mm512_loadu_si512 (s2);		\
    dst1 = _mm512_cmp_epu32_mask (source1.x, source2.x, imm);\
    dst2 = _mm512_mask_cmp_epu32_mask (mask, source1.x, source2.x, imm);\
    if (dst_ref != dst1) abort();			\
    if ((mask & dst_ref) != dst2) abort();
#endif

void
TEST ()
{
    unsigned int s1[16] = {2134,  6678,  453, 54646,
			  231, 5674,  111, 23241,
			  12314,  145,  671, 77575,
			  23455,  166, 5321, 5673};
    unsigned int s2[16] = {41124, 6678, 8653,   856,
			  231, 4646,  111,   124,
			  2745, 4567, 3676,   123,
			  714, 3589, 5683,  5673};
    UNION_TYPE (AVX512F_LEN, i_d) source1, source2;
    MASK_TYPE dst1, dst2, dst_ref;
    MASK_TYPE mask = MASK_VALUE;
    int i;

    CMP(0x00, s1[i] == s2[i]);
    CMP(0x01, s1[i] < s2[i]);
    CMP(0x02, s1[i] <= s2[i]);
    CMP(0x03, 0);
    CMP(0x04, s1[i] != s2[i]);
    CMP(0x05, s1[i] >= s2[i]);
    CMP(0x06, s1[i] > s2[i]);
    CMP(0x07, 1);
}
