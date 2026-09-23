/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64-v4 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */

#ifndef N
#define N 4
#endif

typedef float vector[N];

extern vector v;

extern void foo (vector *);

void
func (vector a, vector b)
{
  vector r;
  for (int i = 0; i < N; i++)
    r[i] = a[i] * b[i];
  foo (&r);
}
