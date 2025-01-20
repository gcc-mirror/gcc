/* { dg-do compile  { target { rv64 } } } */
/* { dg-options "-march=rv64gc_xtheadcondmov -mabi=lp64d -O2" } */

long long int
foo (long long int x, long long int y)
{
  if (((int) x | (int) y) != 0)
    return 6;
  return x + y;
}

/* { dg-final { scan-assembler-times {\msext\.w\M} 1 } } */
