/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-inline" } */

extern void abort (void);

#define N 30
#define M 25089992

int
foonegsi (int a)
{
  return a ? N : -N;
}

/* { dg-final { scan-assembler "csneg\tw\[0-9\]*.*" } } */


int
fooinvsi (int a)
{
  return a ? N : ~N;
}

/* { dg-final { scan-assembler "csinv\tw\[0-9\]*.*" } } */


long long
foonegdi (long long a)
{
  return a ? N : -N;
}

long long
largefooneg (long long a)
{
  return a ? M : -M;
}

/* { dg-final { scan-assembler "csneg\tx\[0-9\]*.*" } } */

long long
fooinvdi (long long a)
{
  return a ? N : ~N;
}

long long
largefooinv (long long a)
{
  return a ? M : ~M;
}

/* { dg-final { scan-assembler "csinv\tx\[0-9\]*.*" } } */


int
main (void)
{
  if (foonegsi (1) != N)
    abort ();

  if (foonegsi (0) != -N)
    abort ();

  if (fooinvsi (1) != N)
    abort ();

  if (fooinvsi (0) != ~N)
    abort ();

  if (foonegdi (1) != N)
    abort ();

  if (foonegdi (0) != -N)
    abort ();

  if (fooinvdi (1) != N)
    abort ();

  if (fooinvdi (0) != ~N)
    abort ();

  if (largefooinv (0) != ~M)
    abort ();

  if (largefooneg (0) != -M)
    abort ();

  if (largefooinv (1) != M)
    abort ();

  if (largefooneg (1) != M)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-not "csel\tx\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-not "csel\tw\[0-9\]*.*" } } */
