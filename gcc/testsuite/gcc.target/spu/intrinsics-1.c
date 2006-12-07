/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */
#include <spu_intrinsics.h>
/* With this intrinsics section, we used to ICE as we would try
   to convert from an vector to an integer type.  */
void f(void)
{
  vec_uint4 gt, N;
  vec_int4 a;
  int *a1;
  _Complex double b;
  gt = spu_cmpgt(a, N); /* { dg-error "parameter list" } */
  gt = spu_cmpgt(a, a1); /* { dg-error "integer from pointer without a cast" } */
  gt = spu_cmpgt(a, b); /* { dg-error "parameter list" } */
  gt = spu_cmpgt(a, a);
  /* Remove this xfail once, we reject implict conversions between vector types.  */
  a = spu_cmpgt(a, a); /* { dg-error "" "" { xfail *-*-* } } */
}
