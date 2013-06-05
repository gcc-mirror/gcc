/* { dg-do compile } */
/* { dg-options "-ftree-vectorize -fcilkplus" } */

int *a, *b;

void foo()
{
  int i;
#pragma simd
  for (i=0; i < 10000; ++i) /* { dg-error "initializer does not declare a var" } */
    a[i] = b[i];
}
