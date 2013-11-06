/* Verify that we generate fmov.d instructions to move doubles when -mfmovd 
   option is enabled.  */
/* { dg-do compile }  */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-mfmovd" } */
/* { dg-skip-if "" { *-*-* }  { "*-single-only" } { "" } } */
/* { dg-final { scan-assembler "fmov.d" } } */

extern double g;

void
f (double d)
{
  g = d;
}

extern float h;

void f2 ()
{
  h = g;
}
