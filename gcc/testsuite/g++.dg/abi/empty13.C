// PR c++/60336
// { dg-do run { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
// { dg-options "-x c -fabi-version=11" }
// { dg-additional-sources "empty13a.c" }
// { dg-additional-options -Wno-complain-wrong-lang }

#include "empty13.h"
extern "C" void fun(struct dummy, struct foo);

int main()
{
  struct dummy d;
  struct foo f = { -1, -2, -3, -4, -5 };

  fun(d, f);
  return 0;
}
