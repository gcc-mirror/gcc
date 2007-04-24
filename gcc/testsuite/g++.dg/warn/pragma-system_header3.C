// PR c++/30500
// { dg-options "-Wcast-align" }

#include "pragma-system_header3.h"

void f()
{
  g<int>();
  h<int>();
}
