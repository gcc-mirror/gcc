/* Verify that we generate fmov.d instructions to move doubles when -mfmovd 
   option is enabled.  */
/* { dg-do compile { target { double_fpu } } }  */
/* { dg-options "-mfmovd" } */
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
