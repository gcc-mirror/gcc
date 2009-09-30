// { dg-options "-std=c++0x" }
#include <cassert>

int main() {
  int i = 1, j = 2;
  const int& ci = i;
  [&ci, &j] () -> void { j = ci; } ();
  assert(i == 1);
  assert(j == 1);
  [&ci] () -> void { ci = 0; } (); // { dg-error "" "cannot assign to const int&" }

  return 0;
}

