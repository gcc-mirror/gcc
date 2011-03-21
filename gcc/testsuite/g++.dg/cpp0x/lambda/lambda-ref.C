// { dg-do run }
// { dg-options "-std=c++0x" }

#include <cassert>

int main() {
  int i = 1;
  float j = 2.0;
  [&i, &j] () -> void { i = 3; j = 4.0; } ();
  assert(i == 3);
  assert(j == 4.0);

  return 0;
}

