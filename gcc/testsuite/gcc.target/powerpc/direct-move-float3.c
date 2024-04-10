/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

/* Test that we generate XSCVDPSP instead of FRSP and XSCVDPSPN when we combine
   a round from double to float and moving the float value to a GPR.  */

union u {
  float f;
  unsigned int ui;
  int si;
};

unsigned int
ui_d (double d)
{
  union u x;
  x.f = d;
  return x.ui;
}

/* { dg-final { scan-assembler     {\mmfvsrwz\M}   } } */
/* { dg-final { scan-assembler     {\mxscvdpsp\M}  } } */
/* { dg-final { scan-assembler-not {\mmfvsrd\M}    } } */
/* { dg-final { scan-assembler-not {\mmtvsrwz\M}   } } */
/* { dg-final { scan-assembler-not {\mmtvsrd\M}    } } */
/* { dg-final { scan-assembler-not {\mxscvdpspn\M} } } */
/* { dg-final { scan-assembler-not {\msrdi\M}      } } */
