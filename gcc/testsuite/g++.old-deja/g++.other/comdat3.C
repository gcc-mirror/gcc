// Test that duplicate elimination of implicit instantiations of static
// data members works properly.

// Additional sources: comdat3-aux.cc
// Additional files: comdat3.h

#include "comdat3.h"

int main ()
{
  const bool *p = &A<int>::b;
  f ();
}
