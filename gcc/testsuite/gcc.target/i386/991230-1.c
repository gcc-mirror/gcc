/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O -ffast-math -mtune=i486" } */

/* Test that floating point greater-than tests are compiled correctly with
   -ffast-math.  */

extern void abort (void);

static int gt (double a, double b)
{
  if (a > b)
    return 4;
  return 0;
}

static double zero = 0.0;

int main ()
{
  if (gt (zero, zero))
    abort ();
  return 0;
}
