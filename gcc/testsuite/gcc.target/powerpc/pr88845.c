/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler {\mmtvsrd\M} { target { lp64 } } } } */
/* { dg-final { scan-assembler {\mxscvspdpn\M} { target { lp64 } } } } */

/* Verify that we do not ICE and that we generate a direct move
   for float types when compiling for 64-bit.  */

struct a {
  unsigned ui;
  float f;
};

void
foo (void)
{
  float e;
  struct a s;
  e = s.f;
  __asm__("" : : "d" (e));
}
