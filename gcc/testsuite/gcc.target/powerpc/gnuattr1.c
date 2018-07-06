/* { dg-do compile { target { powerpc*-linux-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -mlong-double-64" } */
/* { dg-final { scan-assembler "gnu_attribute 4, 9" } } */

/* Check that if we can do the long double operation without doing an emulator
   call, such as with 64-bit long double support, that we still set the
   appropriate .gnu_attribute.  */

long double a;

void add1 (void)
{
  a++;
}
