/* { dg-do compile } */
/* { dg-options "-march=celledp -O1" } */
/* { dg-final { scan-assembler "dfceq" } } */

/* GCC previously transformed an "a <= b" test into "! (a > b)" when
   compiling with -march=celledp, so that the dfcgt instruction can be
   used to implement the comparison.

   However, this transformation violates the IEEE-754 standard in the
   presence of NaN values.  If either a or b is a NaN, a <= b should
   evaluate to false according to IEEE rules.  However, after the
   transformation, a > b as implemented by dfcgt itself returns false,
   so the transformed test returns true.

   Note that the equivalent transformation is valid for single-
   precision floating-point values on the Cell SPU, because the format
   does not have NaNs.  It is invalid for double-precision, even on
   Cell, however.  */

int test (double a, double b) __attribute__ ((noinline));
int test (double a, double b)
{
  return a <= b;
}

int main (void)
{
  double x = 0.0;
  double y = 0.0/0.0;
  return test (x, y);
}
