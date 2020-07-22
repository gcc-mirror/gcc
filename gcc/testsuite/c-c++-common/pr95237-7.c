/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2" { target { i?86-*-* x86_64-*-* } } } */
#include <stddef.h>
struct test
{
  char a;
  long long b;
};
struct test global_var;
int main()
{
  	struct test local_var;
	if (__alignof__(global_var) != 4
	    || __alignof__(local_var) != 4
	    || offsetof(struct test, b) != 4)
	  __builtin_abort();
	return 0;
}
