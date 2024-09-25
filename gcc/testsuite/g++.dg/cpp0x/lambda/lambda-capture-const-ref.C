// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

int main() {
  int i = 1, j = 2;
  const int& ci = i;
  [&ci, &j] () -> void { j = ci; } ();
  assert(i == 1);
  assert(j == 1);
  //[&ci] () -> void { ci = 0; } (); { dg-error: cannot assign to const int& }

  return 0;
}

