/* { dg-do assemble { target avx512fp16 } } */
/* { dg-options "-O3 -march=sapphirerapids" } */

extern long c[];
extern int d[];
long a;

long e (long f)
{
  return f < a ? f : a;
}

void g (void)
{
  for (signed b = 0; b < 4028643; b++)
    d[b] = e ((char) (~c[b]));
}
