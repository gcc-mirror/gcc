/* { dg-do compile } */

#include <stddef.h>

void foo(void);
void test(int (*f)(void), char *p)
{
  __builtin_call_with_static_chain(f(), p);
  __builtin_call_with_static_chain(p, f());  /* { dg-error "must be a call" } */
  __builtin_call_with_static_chain(f() + 1, p); /* { dg-error "must be a call" } */
  __builtin_call_with_static_chain(f(), 0);  /* { dg-error "must be a pointer" } */
  __builtin_call_with_static_chain(f(), NULL);
  __builtin_call_with_static_chain(foo, p);  /* { dg-error "must be a call" } */
  __builtin_call_with_static_chain(foo(), p);
  __builtin_call_with_static_chain(foo(), foo);
}
