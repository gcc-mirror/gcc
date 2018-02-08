// PR c++/60336
// { dg-do run { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
// { dg-options "-Wabi=11 -x c" }
// { dg-additional-sources "empty14a.c" }
// { dg-prune-output "command line option" }

#include "empty14.h"
extern "C" void fun(struct dummy, struct foo);

int main()
{
  struct dummy d;
  struct foo f = { -1, -2, -3, -4, -5 };

  fun(d, f); // { dg-warning "empty" }
  return 0;
}
