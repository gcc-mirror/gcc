/* Check that compare-branch is inverted properly.
   In this case the improved bit test is a side effect of compare-branch
   inversion patterns, even though the branch condition does not get
   inverted here.
   Example:
	mov.b	@(14,r9),r0	->	mov.b	@(14,r9),r0
	shll	r0			cmp/pz	r0
	subc	r0,r0			bt	.L192
	and	#1,r0
	tst	r0,r0
	bt	.L195
*/
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "shll|subc|and" } } */
int
test_00 (int* p)
{
  int nr = 15;
  volatile char* addr = (volatile char*)&p[1];

  if ((addr[(nr >> 3) ^ 7] & (1 << (nr & 7))) == 0)
    return 40;
  else
    return 50;
}
