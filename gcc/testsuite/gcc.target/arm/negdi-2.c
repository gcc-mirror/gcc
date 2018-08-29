/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2" } */

signed long long zero_extendsidi_negsi (unsigned int x)
{
  return -x;
}
/*
Expected output:
	rsb	r0, r0, #0
	mov	r1, #0
*/
/* { dg-final { scan-assembler-times "rsbs?\\t...?, ...?, #0" 1 } } */
/* { dg-final { scan-assembler-times "mov" 1 } } */
