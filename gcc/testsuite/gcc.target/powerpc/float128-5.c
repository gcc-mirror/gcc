/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -mno-float128" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

/* Test that we can use #pragma GCC target to enable -mfloat128 and generate
   code on ISA 3.0 for the float128 built-in functions.  Lp64 is required
   because we need TImode to be available to enable __float128 using hardware
   instructions.  */

#ifdef __FLOAT128__
#error "-mno-float128 should disable initially defining __FLOAT128__"
#endif

#pragma GCC target("float128")

#ifndef __FLOAT128__
#error "#pragma GCC target(\"float128\") should enable -mfloat128"
#endif

__float128
qabs (__float128 a)
{
  return __builtin_fabsf128 (a);
}

/* { dg-final { scan-assembler "xsabsqp"  } } */
