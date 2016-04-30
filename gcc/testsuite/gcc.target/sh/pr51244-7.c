/* Check that compare-branch is inverted properly.
   Example:
	clrt		->	clrt
	subc	r0,r6		subc	r0,r6
	mov	r3,r7		mov	r3,r7
	subc	r1,r7		subc	r1,r7
	mov	#0,r1		tst	r7,r7
	cmp/hi	r1,r7		bf	.L111
	bt	.L111		bra	.L197
	bra	.L197
	nop
*/
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "cmp/hi" } } */
/* { dg-final { scan-assembler-not "mov\t#0" } } */

int other_func (long long);
int
test_00 (unsigned long long a, unsigned long long b)
{
  if ((a - b) > 0xFFFFFFFFLL)
    return other_func (a - b);
  return 20;
}
