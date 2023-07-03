/* { dg-do compile } */
/* { dg-options " -O2 -msse4.1 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times {(?n)mins[sd]} 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {(?n)maxs[sd]} 2 { target { ! ia32 } } } } */
/* Ideally cond_swap_df is also optimized to minsd/maxsd.  */
/* { dg-final { scan-assembler-times {(?n)mins[sd]} 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times {(?n)maxs[sd]} 1 { target ia32 } } } */

void __cond_swap_df(double* __x, double* __y) {
  _Bool __r = (*__x < *__y);
  double __tmp = __r ? *__x : *__y;
  *__y = __r ? *__y : *__x;
  *__x = __tmp;
}

void __cond_swap_sf(float* __x, float* __y) {
  _Bool __r = (*__x < *__y);
  float __tmp = __r ? *__x : *__y;
  *__y = __r ? *__y : *__x;
  *__x = __tmp;
}
