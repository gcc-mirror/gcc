// Build don't link: 
// Since the constructor is in streambuf.h, additional diagnostics are
// produced, which are not really supported in the old-deja framework

#include <sstream>

void
t( char* buf )
{
  std::istrstream str = buf;  //ERROR - inaccessible copy constructor
}

