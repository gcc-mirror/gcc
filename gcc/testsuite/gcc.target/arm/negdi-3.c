/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2" } */

signed long long negdi_zero_extendsidi (unsigned int x)
{
  return -((signed long long) x);
}
/*
Expected output:
        rsbs    r0, r0, #0
        sbc     r1, r1, r1
*/
/* { dg-final { scan-assembler-times "rsb" 1 } } */
/* { dg-final { scan-assembler-times "sbc" 1 } } */
/* { dg-final { scan-assembler-times "mov" 0 } } */
/* { dg-final { scan-assembler-times "rsc" 0 } } */
