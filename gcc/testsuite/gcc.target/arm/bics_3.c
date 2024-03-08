/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */
/* { dg-require-effective-target arm32 } */

int
bics_si_test (int a, int b)
{
  if ((a & ~b) >= 0)
    return 3;
  else
    return 0;
}

int
bics_si_test2 (int a, int b)
{
  if ((a & ~ (b << 2)) >= 0)
    return 3;
  else
    return 0;
}

int
main (void)
{
  int a = 5;
  int b = 5;
  int c = 20;
  if (bics_si_test (a, b) != 3)
    __builtin_abort ();
  if (bics_si_test2 (c, b) != 3)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "bics\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "bics\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+, .sl #2" 1 } } */
