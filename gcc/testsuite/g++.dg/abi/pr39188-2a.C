// PR c++/39188
// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-sources "pr39188-2b.C" }

#include "pr39188-2.h"

int
x (int i)
{
  return f<int> (i);
}
