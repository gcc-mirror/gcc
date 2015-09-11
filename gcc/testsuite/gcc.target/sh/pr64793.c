/* Check that the delay slot of an rts insn is filled, if it follows a cbranch
   with an unfilled delay slot, as in:
	bt	.L3
	mov	r7,r0	<<< this insn
	rts
	nop		<<< should go into this delay slot
*/
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "nop" } }  */

int
test_0 (const char* x, int a, int b, int c)
{
  if (x[a] == 92)
    return b;
  return c;
}
