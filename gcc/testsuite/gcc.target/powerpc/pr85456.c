/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mvsx -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

/* Check that the __builtin_powil generates a call to the correct function
   when long double uses IEEE 128-bit floating point.  */

long double
do_powl (long double a, int i)
{
  return __builtin_powil (a, i);
}

/* { dg-final { scan-assembler "bl __powikf2" { target { powerpc*-*-linux* } } } } */
/* { dg-final { scan-assembler "bl .__powidf2" { target { powerpc*-*-aix* } } } } */
