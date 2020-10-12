/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2" { target { i?86-*-* x86_64-*-* } } } */
int main()
{
  	extern long long x;
	if (__alignof__(x) != 8)
	  __builtin_abort();
	return 0;
}
