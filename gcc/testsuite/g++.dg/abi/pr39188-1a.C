// PR c++/39188
// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-sources "pr39188-1b.C" }

#include "pr39188-1.h"

int
x (int i)
{
  return f (i);
}
