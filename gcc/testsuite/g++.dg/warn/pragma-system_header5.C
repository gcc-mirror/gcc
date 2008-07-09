// PR c++/36760
// { dg-options "-Wextra" }

#include "pragma-system_header5.h"

void f()
{
  g<const double>();
  g<volatile void>();
}
