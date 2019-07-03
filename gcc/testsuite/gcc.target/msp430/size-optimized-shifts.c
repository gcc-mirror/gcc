/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "__mspabi_sral_4" } } */
/* { dg-final { scan-assembler-not "__mspabi_srll_4" } } */
/* { dg-final { scan-assembler-not "__mspabi_slll_4" } } */
/* { dg-final { scan-assembler "__mspabi_sral" } } */
/* { dg-final { scan-assembler "__mspabi_srll" } } */
/* { dg-final { scan-assembler "__mspabi_slll" } } */

/* Ensure that SImode shifts by a constant amount do not use the const_variant
   of the shift library code when optimizing for size.  */

long a;
long b;
long c;
long d;
unsigned long e;
unsigned long f;

void
foo (void)
{
  a = b >> 4;
  c = d << 4;
  e = f >> 4;
}
