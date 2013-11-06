/* Check that compare-branch is inverted properly.
   Example:
	mov	#1,r0	->	tst	r8,r8
	neg	r8,r1		bt	.L47
	shad	r1,r0
	tst	#1,r0
	bf	.L47
*/
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "shad|neg" } } */

int test_01_00 (int*, void*);
int
test_01 (int* m, void* v)
{
  unsigned long n = (unsigned long)v - 1;

  if (!n)
    return 50;
  
  if (1 & (1 << n))	/* if n == 0: 1 & (1 << 0) -> true  */
    return 60;		
  else			/* if n != 0: 1 & (1 << n) -> false  */
    return -8;
}
