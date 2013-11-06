/* Check that for dynamic logical right shifts with a constant the negated
   constant is loaded directly, instead of loading the postitive constant
   and negating it separately.  This was a case that happened at optimization
   level -O2 and looked like:
	cmp/eq	r6,r5
	mov	#30,r1
	neg	r1,r1
	shld	r1,r4
	mov	r4,r0
	rts
	rotcr	r0  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*"} { "-m3* -m2a* -m4*" } }  */
/* { dg-final { scan-assembler-not "neg" } } */

unsigned int
test (unsigned int a, int b, int c)
{
  unsigned char r = b == c;
  return ((a >> 31) | (r << 31));
}
