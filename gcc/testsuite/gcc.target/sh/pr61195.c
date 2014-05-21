/* Verify that we don't switch mode for single moves.  */
/* { dg-do compile }  */
/* { dg-require-effective-target hard_float } */
/* { dg-skip-if "" { *-*-* }  { "mfmovd" } { "" } } */
/* { dg-final { scan-assembler-not "fpscr" } } */

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
