/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops -ffast-math -fvariable-expansion-in-unroller" } */

extern void abort (void);

float array[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

int
foo (int n)
{
  unsigned i;
  float accum = 0;

  for (i = 0; i < n; i++)
    accum += array[i];

  if (accum != 55)
    abort ();

  return 0;
}

int
main (void)
{
  return foo (10);
}


