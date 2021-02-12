// { dg-do compile { target c++11 } }
// PR 98531  Making __cxa_atexit (or atexit) more visible means it
// must be consistent with the std library's declarations

struct C
{
  ~C () noexcept;
  C () noexcept;
};

C &frob ()
{
  static C c; // Requires atexit functionality

  return c;
}

// Make sure this agrees with what we introduced above
#include <cxxabi.h>
#include <cstdlib>
