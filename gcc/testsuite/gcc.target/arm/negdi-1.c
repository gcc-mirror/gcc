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
/* { dg-final { scan-assembler-times "rsb" 1 { target { arm_nothumb } } } } */
/* { dg-final { scan-assembler-times "negs\\t" 1 { target { ! { arm_nothumb } } } } } */
/* { dg-final { scan-assembler-times "asr" 1 } } */
