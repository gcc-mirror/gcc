/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mvsx -mfloat128 -O2" } */

/* PR 85657 -- test __builtin_pack_ibm128.  */

__ibm128
pack (double dummy, double a, double b)
{
  /* Should just generate some moves.  */
  return __builtin_pack_ibm128 (a, b);
}

/* { dg-final { scan-assembler     {\m(fmr|xxlor)\M} } } */
/* { dg-final { scan-assembler-not {\mbl\M} } } */
/* { dg-final { scan-assembler-not {\m(stfd|stxsd)x?\M} } } */
/* { dg-final { scan-assembler-not {\m(lfd|lxsd)x?\M} } } */
/* { dg-final { scan-assembler-not {\m(mtvsrd|mfvsrd)\M} } } */
