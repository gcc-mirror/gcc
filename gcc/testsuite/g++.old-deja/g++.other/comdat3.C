// { dg-do run  }
// { dg-additional-sources " comdat3-aux.cc" }
// Test that duplicate elimination of implicit instantiations of static
// data members works properly.

// Additional files: comdat3.h

#include "comdat3.h"

int main ()
{
  const bool *p = &A<int>::b;
  f ();
}
