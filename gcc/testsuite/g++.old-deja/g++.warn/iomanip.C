// { dg-do assemble  }
// { dg-options "-O -Winline" }
// Bug: g++ wouldn't inline op<< because it was an explicit instantiation.
// Origin: Jason Merrill <jason@cygnus.com>

#include <iomanip>
#include <iostream>

void
f()
{
  std::cout << std::setw(3);
}
