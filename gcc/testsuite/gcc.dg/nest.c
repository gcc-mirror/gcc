/* PR 5967, PR 7114 */
/* { dg-do run } */
/* { dg-options "-O2 -pg" } */

long foo (long x)
{
  long i, sum = 0;
  long bar (long z) { return z * 2; }

  for (i = 0; i < x; i++)
    sum += bar (i);

  return sum;
}

int main (void)
{
  if (foo(10) != 90)
    abort ();
  return 0;
}
