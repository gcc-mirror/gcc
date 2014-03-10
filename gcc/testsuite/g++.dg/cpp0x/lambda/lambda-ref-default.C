// { dg-do run { target c++11 } }

#include <cassert>

int main() {
  int i = 1;
  float j = 2.0;
  [&] () -> void { i = 3; j = 4.0; } ();
  assert(i == 3);
  assert(j == 4.0);

  return 0;
}

