/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */
/* { dg-require-effective-target arm32 } */

extern void abort (void);

int
bics_si_test1 (int a, int b, int c)
{
  int d = a & ~b;

  /* { dg-final { scan-assembler-not "bics\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+" } } */
  /* { dg-final { scan-assembler-times "bic\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+" 2 } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

int
bics_si_test2 (int a, int b, int c)
{
  int d = a & ~(b << 3);

  /* { dg-final { scan-assembler-not "bics\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+, .sl \#3" } } */
  /* { dg-final { scan-assembler "bic\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+, .sl \#3" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

int
main ()
{
  int x;

  x = bics_si_test1 (29, ~4, 5);
  if (x != ((29 & 4) + ~4 + 5))
    abort ();

  x = bics_si_test1 (5, ~2, 20);
  if (x != 25)
    abort ();

  x = bics_si_test2 (35, ~4, 5);
  if (x != ((35 & ~(~4 << 3)) + ~4 + 5))
    abort ();

  x = bics_si_test2 (96, ~2, 20);
  if (x != 116)
    abort ();

  return 0;
}

