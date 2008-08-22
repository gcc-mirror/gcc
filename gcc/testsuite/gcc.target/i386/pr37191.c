/* { dg-do compile } */
/* { dg-options "-O1 -mmmx" } */

#include <mmintrin.h>
#include <stddef.h>
#include <stdint.h>

extern const uint64_t ff_bone;

static inline void transpose4x4(uint8_t *dst, uint8_t *src, ptrdiff_t dst_stride, ptrdiff_t src_stride) {
    __m64 row0 = _mm_cvtsi32_si64(*(unsigned*)(src + (0 * src_stride)));
    __m64 row1 = _mm_cvtsi32_si64(*(unsigned*)(src + (1 * src_stride)));
    __m64 row2 = _mm_cvtsi32_si64(*(unsigned*)(src + (2 * src_stride)));
    __m64 row3 = _mm_cvtsi32_si64(*(unsigned*)(src + (3 * src_stride)));
    __m64 tmp0 = _mm_unpacklo_pi8(row0, row1);
    __m64 tmp1 = _mm_unpacklo_pi8(row2, row3);
    __m64 row01 = _mm_unpacklo_pi16(tmp0, tmp1);
    __m64 row23 = _mm_unpackhi_pi16(tmp0, tmp1);
    *((unsigned*)(dst + (0 * dst_stride))) = _mm_cvtsi64_si32(row01);
    *((unsigned*)(dst + (1 * dst_stride))) = _mm_cvtsi64_si32(_mm_unpackhi_pi32(row01, row01));
    *((unsigned*)(dst + (2 * dst_stride))) = _mm_cvtsi64_si32(row23);
    *((unsigned*)(dst + (3 * dst_stride))) = _mm_cvtsi64_si32(_mm_unpackhi_pi32(row23, row23));
}

static inline void h264_loop_filter_chroma_intra_mmx2(uint8_t *pix, int stride, int alpha1, int beta1)
{
    asm volatile(
        ""
        :: "r"(pix-2*stride), "r"(pix), "r"((long)stride),
           "m"(alpha1), "m"(beta1), "m"(ff_bone)
    );
}

void h264_h_loop_filter_chroma_intra_mmx2(uint8_t *pix, int stride, int alpha, int beta)
{

    uint8_t trans[8*4] __attribute__ ((aligned (8)));
    transpose4x4(trans, pix-2, 8, stride);
    transpose4x4(trans+4, pix-2+4*stride, 8, stride);
    h264_loop_filter_chroma_intra_mmx2(trans+2*8, 8, alpha-1, beta-1);
    transpose4x4(pix-2, trans, stride, 8);
    transpose4x4(pix-2+4*stride, trans+4, stride, 8);
}
