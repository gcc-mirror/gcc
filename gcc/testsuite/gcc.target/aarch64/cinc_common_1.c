/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-inline" } */

extern void abort (void);

int
foosi (int x)
{
  return x > 100 ? x - 2 : x - 1;
}

int
barsi (int x)
{
  return x > 100 ? x + 4 : x + 3;
}

long long
foodi (long long x)
{
  return x > 100 ? x - 2 : x - 1;
}

long long
bardi (long long x)
{
  return x > 100 ? x + 4 : x + 3;
}

/* { dg-final { scan-assembler-times "cs?inc\tw\[0-9\]*" 2 } } */
/* { dg-final { scan-assembler-times "cs?inc\tx\[0-9\]*" 2 } } */

int
main (void)
{
  if (foosi (105) != 103)
    abort ();

  if (foosi (95) != 94)
    abort ();

  if (barsi (105) != 109)
    abort ();

  if (barsi (95) != 98)
    abort ();

  if (foodi (105) != 103)
    abort ();

  if (foodi (95) != 94)
    abort ();

  if (bardi (105) != 109)
    abort ();

  if (bardi (95) != 98)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-not "csel\tx\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-not "csel\tw\[0-9\]*.*" } } */
