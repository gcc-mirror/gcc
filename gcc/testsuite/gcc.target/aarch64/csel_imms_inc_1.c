/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-inline" } */

extern void abort (void);

int
fooinc (int x)
{
  if (x)
    return 1025;
  else
    return 1026;
}

int
fooinc2 (int x)
{
  if (x)
    return 1026;
  else
    return 1025;
}

int
main (void)
{
  if (fooinc (0) != 1026)
    abort ();

  if (fooinc (1) != 1025)
    abort ();

  if (fooinc2 (0) != 1025)
    abort ();

  if (fooinc2 (1) != 1026)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-not "csel\tw\[0-9\]*.*" } } */
