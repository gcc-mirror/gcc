/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=860 -O2" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-final { scan-assembler-not "\\.p2align 4" } } */

volatile int g;
int f(int a, int b)
{
	int i;

	for (i = 0; i < b; i++)
		a += g;
	return a;
}
