/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O -mcpu=power7 -mno-altivec" } */

/* { dg-warning "-mno-altivec disables vsx" "" { target *-*-* } 1 } */

double
foo (double *x, double *y)
{
  double z[2];
  int i;

  for (i = 0; i < 2; i++)
    z[i] = x[i] + y[i];
  return z[0] * z[1];
}

/* { dg-final { scan-assembler-not "xsadddp" } } */
