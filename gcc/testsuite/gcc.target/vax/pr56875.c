/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-final { scan-assembler     "ashq .*,\\\$0xffffffffffffffff," } } */
/* { dg-final { scan-assembler-not "ashq .*,\\\$-1," } } */

void
a (void)
{
	unsigned long i = 1;
	unsigned long long v;

	v = ~ (unsigned long long) 0 << i;
}
