/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mtune=nocona" } */

double x;

void
__attribute__ ((noinline, noclone))
test_fabs (double a)
{
  asm volatile ("" : "+r" (a));
  x = __builtin_fabs (a);
}

void
__attribute__ ((noinline, noclone))
test_neg (double a)
{
  asm volatile ("" : "+r" (a));
  x = -a;
}

int main ()
{
  test_fabs (-1.0);

  if (x != 1.0)
    __builtin_abort ();

  test_neg (-1.0);

  if (x != 1.0)
    __builtin_abort ();

  return 0;
}
