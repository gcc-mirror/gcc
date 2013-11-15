/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

int *a, *b;

void foo()
{
#pragma simd
  for (const int ci=0; ci<1000; ++ci) /* { dg-error "increment of read-only var\|invalid controlling\|invalid increment\|assignment of read" } */
    a[ci] = b[ci];
}
