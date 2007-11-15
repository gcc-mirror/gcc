/* { dg-do compile } */
/* { dg-final { scan-assembler-not "__eqdf2" } } */

/* Ensure double precision comparisons are always inlined.  */

int test (double a, double b) __attribute__((noinline));
int test (double a, double b)
{
  return a == b;
}
