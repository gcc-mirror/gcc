/* PR target/98522 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

typedef int T;

#define M 2

extern T a[M], b[M], s1[M], s2[M], r[M];

void foo (void)
{
  int j;

  for (j = 0; j < M; j++)
    r[j] = (a[j] < b[j]) ? s1[j] : s2[j];
}

/* { dg-final { scan-assembler "pcmpgtd" { xfail *-*-* } } } */
