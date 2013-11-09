/* Check that compare-branch is inverted properly.
   Example:
	mov.w	.L566,r2	->	mov.w	.L566,r2
	add	r11,r2			add	r11,r2
	mov.l	@(12,r2),r7		mov.l	@(8,r2),r5
	mov.l	@(8,r2),r5		mov.l	@(12,r2),r2
	mov	#0,r2			tst	r2,r2
	cmp/hi	r2,r7			bt	.L534
	bf	.L534
*/
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "mov\t#0" } } */
static inline unsigned int
test_03_00 (unsigned int x)
{
  /* Return unassigned value on purpose.  */
  unsigned int res;
  return res;
}

struct S
{
  unsigned int a;
  unsigned int b;
};

int test_03 (struct S* i)
{
 if ((i->a != 2 && i->a != 3) || i->a > test_03_00 (i->b))
   return -5;

 return -55;
}
