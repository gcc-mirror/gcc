/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort ();

__attribute__((__target__("general-regs-only")))
int
dec (int a, int b)
{
  return a + b;
}

__attribute__((__target__("general-regs-only")))
int
cal (int a, int b)
{
  int sum1 = a * b;
  int sum2 = a / b;
  int sum = dec (sum1, sum2);
  return a + b + sum + sum1 + sum2;
}

__attribute__((__target__("general-regs-only")))
int
main (int argc, char **argv)
{
  int ret = cal (2, 1);

  if (ret != 11)
    abort ();

  return 0;
}
