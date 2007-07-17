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
  a = spu_cmpgt(a, a); /* { dg-message "note: use -flax-vector-conversions to permit conversions between vectors with differing element types or numbers of subparts" } */ 
/* { dg-error "incompatible types in" "" { target *-*-* } 16 } */
}
