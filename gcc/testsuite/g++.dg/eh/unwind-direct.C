// PR target/59788
// { dg-do run { target { *-*-solaris2* && { ! gld } } } }
// { dg-options "-Wl,-Bdirect" }

#include <stdexcept>

int
main(void)
{
  try
    { throw std::runtime_error( "Catch me if you can!"); }
  catch(...)
    { return 0; }
  return 1;
}
