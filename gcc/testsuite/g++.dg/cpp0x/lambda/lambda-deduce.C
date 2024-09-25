// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

int main() {
  [] {};
  [] {} ();
  [] () {};
  [] () {} ();
  [] () { return "lambda"; };

  int i = 1, j = 2;
  [&i, j] () { i = j; } ();
  assert(i == 2);
  assert(j == 2);

  i = [] () { return 3; } ();
  assert(i == 3);

  int k = [&] () { return i; } ();

  []{ return; };

  int array[] = { 1, 2, 3 };
  int* p = [&] () { return array; } ();

  return 0;
}

