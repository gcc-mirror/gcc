// Origin: Dodji Seketeli <dodji@redhat.com>
// { dg-options "-std=c++1y -pedantic-errors" }
// { dg-do compile } 

#include "../system-binary-constants-1.h"

int
foo (void)
{
#if BINARY_INT_CONSTANT_IN_SYSTEM_HEADER
  return 23;
#endif
  return 0b1101; // { dg-bogus "binary constants are a C..1y feature or GCC extension" }
}
