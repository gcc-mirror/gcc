// PR c++/30500
// { dg-options "-Wconversion" }

#include "pragma-system_header1.h"

void f()
{
  g<int>();
  h<int>();
}
