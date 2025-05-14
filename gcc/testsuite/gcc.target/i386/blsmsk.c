/* { dg-do compile } */
/* { dg-options "-O2 -march=znver5" } */
int
test(int a)
{
	return (a - 1)^a;
}
/* { dg-final { scan-assembler-times "blsmsk\[ \\t\]+" 1 } } */
/* { dg-final { scan-assembler-times "xor" 1 } } */
