/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

unsigned int foo (unsigned int u)
{
  unsigned int ret;
  __asm__ ("xxlor %x0,%x1,%x1\t# v, v constraints" : "=v" (ret) : "v" (u));
  return ret;
}

/* { dg-final { scan-assembler "mtvsrwz" } } */
/* { dg-final { scan-assembler "mfvsrwz" } } */
