/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

/* Verify that large integer constants are loaded via direct move instead of being
   loaded from memory.  */

#ifndef _ARCH_PPC64
#error "This code is 64-bit."
#endif

double
p9_large (void)
{
  long l = 0x12345678;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wi" (l));

  return ret;
}

/* { dg-final { scan-assembler     {\mmtvsrd\M} } } */
/* { dg-final { scan-assembler-not {\mld\M}     } } */
/* { dg-final { scan-assembler-not {\mlfd\M}    } } */
/* { dg-final { scan-assembler-not {\mlxsd\M}   } } */
