/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2" } */

signed long long negdi_extendsidi (signed int x)
{
  return -((signed long long) x);
}
/*
Expected output:
        rsbs    r0, r0, #0
        mov     r1, r0, asr #31
*/
/* { dg-final { scan-assembler-times "rsb" 1 } } */
/* { dg-final { scan-assembler-times "asr" 1 } } */
/* { dg-final { scan-assembler-times "rsc" 0 } } */
