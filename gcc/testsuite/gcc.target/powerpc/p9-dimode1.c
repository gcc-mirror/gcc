/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify P9 changes to allow DImode into Altivec registers, and generate
   constants using XXSPLTIB.  */

double
p9_zero (void)
{
  long l = 0;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wa" (l));

  return ret;
}

double
p9_plus_1 (void)
{
  long l = 1;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wa" (l));

  return ret;
}

double
p9_minus_1 (void)
{
  long l = -1;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wa" (l));

  return ret;
}

/* { dg-final { scan-assembler     {\mxxspltib\M} } } */
/* { dg-final { scan-assembler-not {\mmtvsr}      } } */
/* { dg-final { scan-assembler-not {\mlfd\M}      } } */
/* { dg-final { scan-assembler-not {\mld\M}       } } */
/* { dg-final { scan-assembler-not {\mlxsd\M}     } } */
