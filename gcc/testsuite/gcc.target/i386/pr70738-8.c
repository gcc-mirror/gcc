/* { dg-do run } */
/* { dg-options "-O2 -mgeneral-regs-only" } */

extern void abort ();

int
dec (int a, int b)
{
  return a + b;
}

int
cal (int a, int b)
{
  int sum1 = a * b;
  int sum2 = a / b;
  int sum = dec (sum1, sum2);
  return a + b + sum + sum1 + sum2;
}

int
main (int argc, char **argv)
{
  int ret = cal (2, 1);

  if (ret != 11)
    abort ();

  return 0;
}
