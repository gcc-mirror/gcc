/* { dg-do compile { target { powerpc*-linux-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -mabi=ibmlongdouble -Wno-psabi" } */
/* { dg-final { scan-assembler "gnu_attribute 4, 5" } } */

/* Check that if we can do the long double operation without doing an emulator
   call, such as with copysign, that we still set the appropriate
   .gnu_attribute.  */

long double a, b, c;

void cs (void)
{
  a = __builtin_copysignl (b, c);
}
