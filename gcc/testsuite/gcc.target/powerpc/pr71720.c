/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

/* Verify that we generate xxspltw <reg>,<reg>,0 for V4SFmode splat.  */

vector float
splat_v4sf (float f)
{
  return (vector float) { f, f, f, f };
}

/* { dg-final { scan-assembler "xscvdpspn "      } } */
/* { dg-final { scan-assembler "xxspltw .*,.*,0" } } */
