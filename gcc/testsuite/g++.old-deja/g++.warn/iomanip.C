// Bug: g++ wouldn't inline op<< because it was an explicit instantiation.
// Origin: Jason Merrill <jason@cygnus.com>
// Special g++ Options: -O -Winline
// Build don't link:

#include <iomanip>
#include <iostream>

void
f()
{
  std::cout << std::setw(3);
}
