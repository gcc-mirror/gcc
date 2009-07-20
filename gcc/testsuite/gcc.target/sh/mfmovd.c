/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-mfmovd" } */
/* { dg-skip-if "No double precision FPU support" { "sh*-*-*" } "-m2a-nofpu -m2a-single-only -m4-nofpu -m4-single-only -m4a-nofpu -m4a-single-only" { "" } }  */
/* { dg-final { scan-assembler "fmov.d"} }  */

extern double g;

void
f (double d)
{
  g = d;
}

