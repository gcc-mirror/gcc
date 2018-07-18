// PR c++/35652
// { dg-options "-O -Wall" }

#include <string>
int test() {
  // blank line padding, could also be code...
  //
  //
  //
  //
  //
  //
  //
  //
  //
  std::string s = "";
  s += 'x' + "y";	      // { dg-warning "bounds of constant string" }

  return 0;
}

// With -std=c++17 we get another warning deep under operator+=.
// { dg-prune-output __builtin_memcpy }
