/* { dg-do compile } */
/* { dg-options "-std=c11 -O2 -fdump-tree-forwprop1 -fdump-tree-optimized" } */

#include <complex.h>

extern void push1(void *p, float _Complex x);
void foo (void *q, float _Complex *x)
{
  float r = __real *x;
  float i = __imag *x;
  push1 (q, CMPLXF (r, i));
}

/* { dg-final { scan-tree-dump-not "COMPLEX_EXPR" "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "REALPART_EXPR" "optimized" } } */
