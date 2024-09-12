// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

int main() {
  int i = 1, j = 2;
  [&i, j] () mutable -> void { i = 0; j = 0; } ();
  assert(i == 0);
  assert(j == 2);

  return 0;
}

