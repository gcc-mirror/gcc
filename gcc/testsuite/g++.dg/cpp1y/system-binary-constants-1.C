// Origin: Dodji Seketeli <dodji@redhat.com>
// { dg-do compile { target c++14 } }

#include "../system-binary-constants-1.h"

int
foo (void)
{
#if BINARY_INT_CONSTANT_IN_SYSTEM_HEADER
  return 23;
#endif
  return 0b1101; // { dg-bogus "binary constants are a C..14 feature or GCC extension" }
}
