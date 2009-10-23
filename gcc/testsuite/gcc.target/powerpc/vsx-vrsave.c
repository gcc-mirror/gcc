/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler-times "mtvrsave" 2 } } */

/* Check whether VRSAVE is set to non-zero if VSX vector operations were
   used, but it should not be set if there are no vector operations.  */

void
generates_vrsave (vector double *a, vector double *b, vector double *c)
{
  *a = *b + *c;
}

void
no_vrsave (double *a, double *b, double *c)
{
  *a = *b + *c;
}
