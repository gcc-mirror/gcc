/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Verify P9 changes to allow DImode into Altivec registers, and generate
   constants using XXSPLTIB.  */

#ifndef _ARCH_PPC64
#error "This code is 64-bit."
#endif

double
p9_zero (void)
{
  long l = 0;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wi" (l));

  return ret;
}

double
p9_plus_1 (void)
{
  long l = 1;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wi" (l));

  return ret;
}

double
p9_minus_1 (void)
{
  long l = -1;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wi" (l));

  return ret;
}

/* { dg-final { scan-assembler     {\mxxspltib\M} } } */
/* { dg-final { scan-assembler-not {\mmtvsrd\M}   } } */
/* { dg-final { scan-assembler-not {\mlfd\M}      } } */
/* { dg-final { scan-assembler-not {\mld\M}       } } */
/* { dg-final { scan-assembler-not {\mlxsd\M}     } } */
