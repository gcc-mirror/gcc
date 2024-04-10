/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

unsigned int foo (unsigned char u)
{
  unsigned int ret;
  __asm__ ("xxlor %x0,%x1,%x1\t# v, v constraints" : "=v" (ret) : "v" (u));
  return ret;
}

/* { dg-final { scan-assembler "mtvsrwz" } } */
/* { dg-final { scan-assembler "mfvsrwz" } } */
