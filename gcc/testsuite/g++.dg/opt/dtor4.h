#include <cassert>

struct S
{
  int a, i;
  S () : i(1) {}
  __attribute__((noinline)) ~S () { assert (i == 1); }
};
