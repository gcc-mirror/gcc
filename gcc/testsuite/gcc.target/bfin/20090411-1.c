/* { dg-do compile { target bfin-*-* } } */
/* { dg-options "-O2" } */

typedef short __v2hi __attribute__ ((vector_size (4)));
typedef __v2hi raw2x16;
typedef raw2x16 fract2x16;
typedef short fract16;
typedef struct complex_fract16
{
  fract16 re;
  fract16 im;
} __attribute__ ((aligned (4))) complex_fract16;


__inline__ __attribute__ ((always_inline))
     static complex_fract16 csqu_fr16 (complex_fract16 _a)
{
  complex_fract16 _x;
  fract2x16 i =
    __builtin_bfin_csqu_fr16 (__builtin_bfin_compose_2x16 ((_a).im, (_a).re));
  (_x).re = __builtin_bfin_extract_lo (i);
  (_x).im = __builtin_bfin_extract_hi (i);
  return _x;
}

complex_fract16 f (complex_fract16 _a)
{
  return csqu_fr16 (_a);
}
