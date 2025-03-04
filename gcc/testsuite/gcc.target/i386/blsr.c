/* { dg-do compile } */
/* { dg-options "-O2 -march=znver5" } */
void bar ();
int
test(int a)
{
	return a & (a - 1);
}
int
test2(int a)
{
	if (a & (a - 1))
		bar ();
}
int
test3(int a)
{
	int ret = a & (a - 1);
	if (ret)
		bar ();
	return ret;
}
/* All three functions should produce bslr.
   Only test and test3 needs xor to break false dependency.  */
/* { dg-final { scan-assembler-times "blsr\[ \\t\]+" 3 } } */
/* { dg-final { scan-assembler-times "xor" 2 } } */
