/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=4" { target { i?86-*-* x86_64-*-* } } } */
int main()
{
	long long x;
	if (__alignof__(x) != 8)
	  __builtin_abort();
	return 0;
}
