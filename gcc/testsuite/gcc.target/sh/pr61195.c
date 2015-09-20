/* Verify that we don't switch mode for single moves.  */
/* { dg-do compile { target { any_fpu && { ! fmovd_enabled } } } }  */
/* { dg-final { scan-assembler-not "fpscr|fpchg" } } */

float *g;

float
foo(float f)
{
  return f;
}

float
foo1(void)
{
  return *g;
}
