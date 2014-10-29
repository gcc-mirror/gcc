/* { dg-do assemble } */
/* { dg-options "-O2 -mfix-cortex-a53-835769" } */

int
test (int a, double b, int c, int d, int e)
{
  double result;
  __asm__ __volatile ("// %0, %1"
                      : "=w" (result)
                      : "0" (b)
                      :    /* No clobbers */
                      );
  return c * d + e;
}
