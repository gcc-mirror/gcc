// Build don't link: 
// Since the constructor is in streambuf.h, additional diagnostics are
// produced, which are not really supported in the old-deja framework
// excess errors test - XFAIL *-*-*
#include <strstream.h>

void
t( char* buf )
{
  istrstream str = buf;  //ERROR - inaccessible copy constructor
}

