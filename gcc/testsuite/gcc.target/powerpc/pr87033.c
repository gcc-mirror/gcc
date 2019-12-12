/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2" } */

/* Insure that a LWAX is generated instead of ADD + LWA.  LP64 is needed
   because the LWA and LWAX instructions are only available in 64-bit mode.  */
long func (int *p, unsigned long n)
{
  return p[n];
}

/* { dg-final { scan-assembler     {\mlwax\M} } } */
/* { dg-final { scan-assembler-not {\mlwa\M}  } } */
