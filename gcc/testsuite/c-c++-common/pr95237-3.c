/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2" { target { i?86-*-* x86_64-*-* } } } */
int main()
{
	long long x;
	if (__alignof__(x) != 4)
	  __builtin_abort();
	return 0;
}
