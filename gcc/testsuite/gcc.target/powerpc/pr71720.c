/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

/* Verify that we generate xxspltw <reg>,<reg>,0 for V4SFmode splat.  */

vector float
splat_v4sf (float f)
{
  return (vector float) { f, f, f, f };
}

/* { dg-final { scan-assembler "xscvdpspn "      } } */
/* { dg-final { scan-assembler "xxspltw .*,.*,0" } } */
