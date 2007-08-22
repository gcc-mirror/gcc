/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -ffast-math -march=i686" } */


/* Sanity check for fp_jcc_* with TARGET_CMOVE.  */

extern void abort (void);

static int test(double a)
{
  if (a)
    return 0;
}

static double zero = 0.0;

int main ()
{
  test (zero);
  return 0;
}
