/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort ();

__attribute__((__target__("general-regs-only")))
int
cal (int a, int b)
{
  int sum = a + b;
  int sum1 = a * b;
  return (a + b + sum + sum1);
}

__attribute__((__target__("general-regs-only")))
int
main (int argc, char **argv)
{
  int ret = cal (1, 2);

  if (ret != 8)
    abort ();

  return 0;
}
