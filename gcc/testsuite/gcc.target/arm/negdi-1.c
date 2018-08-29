/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2" } */

signed long long extendsidi_negsi (signed int x)
{
  return -x;
}

/*
Expected output:
	rsb	r0, r0, #0
	mov	r1, r0, asr #31
*/
/* { dg-final { scan-assembler-times "rsbs?\\t...?, ...?, #0" 1 } } */
/* { dg-final { scan-assembler-times "asr" 1 } } */
