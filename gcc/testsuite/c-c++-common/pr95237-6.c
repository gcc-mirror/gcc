/* { dg-do run { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2" } */
#include <stddef.h>
#ifdef  __x86_64__
# define EXP_ALIGN 8
#else
# define EXP_ALIGN 4
#endif

struct test
{
  char a;
  long long b;
};
struct test global_var;
int main()
{
  	struct test local_var;
	if (__alignof__(global_var) != EXP_ALIGN
	    || __alignof__(local_var) != EXP_ALIGN
	    || offsetof(struct test, b) != EXP_ALIGN)
	  __builtin_abort();
	return 0;
}
