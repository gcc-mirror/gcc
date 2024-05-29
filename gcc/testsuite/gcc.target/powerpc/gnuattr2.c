/* { dg-do compile { target { powerpc*-linux-* && lp64 } } } */
/* { dg-options "-O2 -mvsx -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "gnu_attribute 4, 13" } } */

/* Check that if we can do the long double operation without doing an emulator
   call, such as with IEEE 128-bit hardware support on power9, that we still
   set the appropriate .gnu_attribute.  The && lp64 is needed, because we can't
   enable the IEEE 128-bit hardware instructions on ISA 3.0 (power9) in 32-bit,
   because we don't have a TImode available.  */

long double a;

void add1 (void)
{
  a++;
}
