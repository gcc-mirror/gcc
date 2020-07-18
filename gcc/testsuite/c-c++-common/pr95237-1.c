/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2" { target { i?86-*-* x86_64-*-* } } } */
typedef __UINTPTR_TYPE__ uintptr_t;
void __attribute__((noipa)) foo (long long *p, uintptr_t a)
{
  if ((uintptr_t)p & (a-1))
      __builtin_abort ();
}
int main()
{
	long long x;
	uintptr_t a = __alignof__(x);
	foo(&x, a);
	return 0;
}
