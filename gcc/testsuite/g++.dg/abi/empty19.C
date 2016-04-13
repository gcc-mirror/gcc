// PR c++/60336
// { dg-do run }
// { dg-options "-Wabi=9 -x c" }
// { dg-additional-sources "empty19a.c" }
// { dg-prune-output "command line option" }

#include "empty19.h"
extern "C" void fun(struct dummy, struct foo);

int main()
{
  struct dummy d;
  struct foo f = { -1, -2, -3, -4, -5 };

  fun(d, f); // { dg-warning "empty" }
  return 0;
}
