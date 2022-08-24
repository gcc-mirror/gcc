/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64 -mtune=skylake -Wno-attributes" } */

#include <x86intrin.h>
#include <stdint.h>

__attribute__((always_inline, target("avx2")))
static __m256i
load8bit_4x4_avx2(const uint8_t *const src, const uint32_t stride)
{
  __m128i src01, src23 = _mm_setzero_si128();
  src01 = _mm_cvtsi32_si128(*(int32_t*)(src + 0 * stride));
  src23 = _mm_insert_epi32(src23, *(int32_t *)(src + 3 * stride), 1);
  return _mm256_setr_m128i(src01, src23);
}

__attribute__ ((noinline, noipa, target("avx2")))
uint32_t
compute4x_m_sad_avx2_intrin(uint8_t *src, uint32_t src_stride,
			    uint8_t *ref, uint32_t ref_stride,
			    uint32_t height)
{
  __m128i xmm0;
  __m256i ymm = _mm256_setzero_si256();
  uint32_t y;

  for (y = 0; y < height; y += 4) {
    const __m256i src0123 = load8bit_4x4_avx2(src, src_stride);
    const __m256i ref0123 = load8bit_4x4_avx2(ref, ref_stride);
    ymm = _mm256_add_epi32(ymm, _mm256_sad_epu8(src0123, ref0123));
    src += src_stride << 2;
    ref += ref_stride << 2;
  }

  xmm0 = _mm_add_epi32(_mm256_castsi256_si128(ymm),
		       _mm256_extracti128_si256(ymm, 1));

  return (uint32_t)_mm_cvtsi128_si32(xmm0);
}  

/* Expect assembly like:

	vextracti128	$0x1, %ymm3, %xmm3
	vpaddd	%xmm3, %xmm0, %xmm0
	vmovd	%xmm0, %eax
	vzeroupper

rather than:

	vzeroupper
	vextracti128	$0x1, %ymm3, %xmm3
	vpaddd	%xmm3, %xmm0, %xmm0
	vmovd	%xmm0, %eax

 */

/* { dg-final { scan-assembler "\[ \t\]+vextracti128\[ \t\]+\[^\n\]+\n\[ \t\]+vpaddd\[ \t\]+\[^\n\]+\n\[ \t\]+vmovd\[ \t\]+\[^\n\]+\n\[ \t\]+vzeroupper" } } */
