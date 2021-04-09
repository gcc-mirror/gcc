/* PR middle-end/97487 */
/* { dg-do compile } */
/* { dg-options "-O2 --param max-rtl-if-conversion-unpredictable-cost=0 -Wno-psabi -w" } */

typedef long long int V __attribute__((vector_size (16)));

long long int
foo (V x, V y)
{
  long long int t1 = y[0];
  long long int t2 = x[0];
  long long int t3;
  if (t2 < 0)
    t3 = t1;
  else
    t3 = 0;
  return t3;
}
