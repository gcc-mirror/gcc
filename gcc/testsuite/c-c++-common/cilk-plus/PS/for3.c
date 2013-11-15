/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

int *a, *c;

void foo()
{
  int i, j;

  // Pointers are OK.
  #pragma simd
  for (int *i=c; i < c; ++i)
    *a = '5';
}
