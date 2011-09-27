/* Verify that we generate fmov.d instructions to move doubles when -mfmovd 
   option is enabled.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-mfmovd" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a" "-m2a-single" "-m4" "-m4-single" "-m4-100" "-m4-100-single" "-m4-200" "-m4-200-single" "-m4-300" "-m4-300-single" "-m4a" "-m4a-single" } }  */
/* { dg-final { scan-assembler "fmov.d" } } */

extern double g;

void
f (double d)
{
  g = d;
}

