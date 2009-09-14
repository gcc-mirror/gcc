/* { dg-do compile { target bfin-*-* } } */

typedef short fract16;
typedef short __v2hi __attribute__ ((vector_size (4)));
typedef __v2hi raw2x16;
typedef raw2x16 fract2x16;
typedef struct complex_fract16 {
  fract16 re;
  fract16 im;
} __attribute__((aligned(4))) complex_fract16;


__inline__
__attribute__ ((always_inline))
static complex_fract16 cmlt_fr16 (complex_fract16 _a,
				  complex_fract16 _b)
{
  complex_fract16 r;
  fract2x16 i;

  i = __builtin_bfin_cmplx_mul(__builtin_bfin_compose_2x16((_a).im, (_a).re),
			       __builtin_bfin_compose_2x16((_b).im, (_b).re));
  (r).re = __builtin_bfin_extract_lo(i);
  (r).im = __builtin_bfin_extract_hi(i);
  return r;
}


complex_fract16 f(complex_fract16 a, complex_fract16 b) {
  return cmlt_fr16(a, b);
}
