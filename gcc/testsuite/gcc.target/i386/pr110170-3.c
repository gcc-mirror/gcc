/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fno-if-conversion -fno-if-conversion2" } */
/* { dg-final { scan-assembler-not {(?n)movq.*r} } } */

void __cond_swap(double* __x, double* __y) {
  _Bool __r = (*__x < *__y);
  double __tmp = __r ? *__x : *__y;
  *__y = __r ? *__y : *__x;
  *__x = __tmp;
}

