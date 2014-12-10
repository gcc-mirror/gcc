/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ev67" } */

typedef short INT16;
typedef unsigned int CARD32;
typedef unsigned short CARD16;
typedef unsigned char CARD8;
typedef struct _Picture *PicturePtr;
typedef int FbStride;
typedef unsigned long __m64;
extern __m64 load8888 (__m64);
static __inline __m64 _mm_setzero_si64(void)
{
  return (__m64)0L;
}
static __inline __m64 _mm_adds_pu8(__m64 __m1, __m64 __m2)
{
    return __m1 + __builtin_alpha_minsb8(__m2, ~__m1);
}
static __inline __m64 _mm_packs_pu16(__m64 __m1, __m64 __m2)
{
    __m1 = __builtin_alpha_minuw4(__m1, 0x00ff00ff00ff00ff);
    __m2 = __builtin_alpha_minuw4(__m2, 0x00ff00ff00ff00ff);
    return __m1 | (__m2 << 32);
}
typedef unsigned long long ullong;
static __inline__ __m64 pix_multiply(__m64 a)
{
    if (a)
	return a;
}
static __inline__ __m64 over(__m64 src, __m64 srca, __m64 dest)
{
    return _mm_adds_pu8(src, pix_multiply(dest));
}

void fbCompositeSolid_nx8888mmx(CARD8 op, PicturePtr pSrc, PicturePtr pMask,
				INT16 yDst, CARD16 width, CARD16 height)
{
    CARD32 src;
    CARD32 *dstLine, *dst;
    CARD16 w;
    FbStride dstStride;
    __m64 vsrc, vsrca;
    vsrc = load8888(src);
    while (height--) {
	dst = dstLine;
	dstLine += dstStride;
	while (w && (unsigned long) dst & 7) {
	    *dst = _mm_packs_pu16(_mm_adds_pu8(vsrc, load8888(*dst)),
				  _mm_setzero_si64());
	    dst++;
	}
	while (w >= 2) {
	    __m64 dest0, dest1;
	    *(__m64 *) dst = _mm_packs_pu16(dest0, dest1);
	    w -= 2;
	}
	while (w) {
	    *dst = _mm_packs_pu16(_mm_adds_pu8(vsrc, pix_multiply(0)), 0);
	    w--;
	}
    }
}
