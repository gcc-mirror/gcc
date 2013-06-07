/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

int *a, *b;

void foo()
{
#pragma simd
  for (int i=100; i; --i)
    a[i] = b[i];
}
