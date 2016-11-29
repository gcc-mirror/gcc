/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mvsx-small-integer" } */

unsigned int foo (unsigned char u)
{
  unsigned int ret;
  __asm__ ("xxlor %x0,%x1,%x1\t# v, v constraints" : "=v" (ret) : "v" (u));
  return ret;
}

/* { dg-final { scan-assembler "mtvsrwz" } } */
/* { dg-final { scan-assembler "mfvsrwz" } } */
