/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#include <math.h>
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

#if AVX512F_LEN == 512
#undef CMP
#define CMP(imm, rel)					\
    dst_ref = 0;					\
    for (i = 0; i < 64; i++)				\
    {							\
      dst_ref = ((rel) << i) | dst_ref;			\
    }							\
    source1.x = _mm512_loadu_si512 (s1);		\
    source2.x = _mm512_loadu_si512 (s2);		\
    dst1 = _mm512_cmp_epu8_mask (source1.x, source2.x, imm);\
    dst2 = _mm512_mask_cmp_epu8_mask (mask, source1.x, source2.x, imm);\
    if (dst_ref != dst1) abort();			\
    if ((mask & dst_ref) != dst2) abort();
#endif

#if AVX512F_LEN == 256
#undef CMP
#define CMP(imm, rel)					\
    dst_ref = 0;					\
    for (i = 0; i < 32; i++)				\
    {							\
      dst_ref = ((rel) << i) | dst_ref;			\
    }							\
    source1.x = _mm256_loadu_si256 ((__m256i*)s1);	\
    source2.x = _mm256_loadu_si256 ((__m256i*)s2);	\
    dst1 = _mm256_cmp_epu8_mask (source1.x, source2.x, imm);\
    dst2 = _mm256_mask_cmp_epu8_mask (mask, source1.x, source2.x, imm);\
    if (dst_ref != dst1) abort();			\
    if ((mask & dst_ref) != dst2) abort();
#endif

#if AVX512F_LEN == 128
#undef CMP
#define CMP(imm, rel)					\
    dst_ref = 0;					\
    for (i = 0; i < 16; i++)				\
    {							\
      dst_ref = ((rel) << i) | dst_ref;			\
    }							\
    source1.x = _mm_loadu_si128 ((__m128i*)s1);		\
    source2.x = _mm_loadu_si128 ((__m128i*)s2);		\
    dst1 = _mm_cmp_epu8_mask (source1.x, source2.x, imm);\
    dst2 = _mm_mask_cmp_epu8_mask (mask, source1.x, source2.x, imm);\
    if (dst_ref != dst1) abort();			\
    if ((mask & dst_ref) != dst2) abort();
#endif

void
TEST ()
{
    unsigned char s1[64] = {34, 78,  53, 64,
			    1, 57, 11, 231,
			    14, 45, 71, 75,
			    55, 66, 21, 73,
			    34, 68, 3, 56,
			    1, 57, 111, 241,
			    14,  15, 61, 75,
			    55,  16, 52, 3,
			    34, 78,  53, 64,
			    1, 57, 11, 231,
			    14, 45, 71, 75,
			    55, 66, 21, 73,
			    34, 68, 3, 56,
			    1, 57, 111, 241,
			    14,  15, 61, 75,
			    55,  16, 52, 3};
    unsigned char s2[64] = {4, 68, 86, 8,
			    1, 46,  1, 1,
			    45, 67, 36, 3,
			    4, 39, 56, 56,
			    124, 78, 53, 56,
			    1, 46,  1, 12,
			    45, 47, 36, 13,
			    4, 35, 56, 67,
			    4, 68, 86, 8,
			    1, 46,  1, 1,
			    45, 67, 36, 3,
			    4, 39, 56, 56,
			    124, 78, 53, 56,
			    1, 46,  1, 12,
			    45, 47, 36, 13,
			    4, 35, 56, 67};
    UNION_TYPE (AVX512F_LEN, i_b) source1, source2;
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
