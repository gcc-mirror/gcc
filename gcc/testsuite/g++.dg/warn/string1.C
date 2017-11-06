// PR c++/35652
// { dg-options "-O" }

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
