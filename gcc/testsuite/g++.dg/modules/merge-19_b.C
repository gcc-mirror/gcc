// PR c++/121238
// { dg-module-do run }
// { dg-additional-options "-fmodules -fno-module-lazy" }

#include "merge-19.h"
import "merge-19_a.H";

int main() {
  const char fmt[] = "5";
  if (!parse_integer<void>(fmt))
    __builtin_abort();

  S s{ 5 };
  if (!take_by_invisiref(s))
    __builtin_abort();
}
