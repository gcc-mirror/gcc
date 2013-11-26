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
/* { dg-final { scan-assembler-times "rsb\\t...?, ...?, #0" 1 { target { arm_nothumb } } } } */
/* { dg-final { scan-assembler-times "negs\\t...?, ...?" 1 { target { ! arm_nothumb } } } } */
/* { dg-final { scan-assembler-times "mov" 1 } } */
