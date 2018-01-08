/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */
/* { dg-require-effective-target arm32 } */
extern void abort (void);
int
bics_si_test1 (int a, int b, int c)
{
  if ((a | b) == a)
    return a;
  else
    return c;
}

int
bics_si_test2 (int a, int b, int c)
{
  if ((a | b) == b)
    return b;
  else
    return c;
}

int
main ()
{
  int x;
  x = bics_si_test1 (0xf00d, 0xf11f, 0);
  if (x != 0)
    abort ();

  x = bics_si_test1 (0xf11f, 0xf00d, 0);
  if (x != 0xf11f)
    abort ();

  x = bics_si_test2 (0xf00d, 0xf11f, 0);
  if (x != 0xf11f)
    abort ();

  x = bics_si_test2 (0xf11f, 0xf00d, 0);
  if (x != 0)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "bics\tr\[0-9\]+, r\[0-9\]+, r\[0-9\]+" 2 } } */
