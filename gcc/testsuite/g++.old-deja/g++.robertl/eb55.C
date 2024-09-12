// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for sstream" { ! hostedlib } }
// Since the constructor is in streambuf.h, additional diagnostics are
// produced, which are not really supported in the old-deja framework

#include <sstream>

void
t( char* buf )
{
  std::istrstream str = buf;  //{ dg-error "" } inaccessible copy constructor
}

