// { dg-do run { target c++11 } }
#include <cassert>

int main() {
  int i = 1;
  const char* s1 = "hello";
  const char* s2 = s1;
  [i, s2] () mutable -> void { i = 2; s2 = "world"; } ();
  //[i, s2] () -> void { i = 2; s2 = "world"; } (); // { dg-error: "assignment of data-member in read-only structure" }
  assert(i == 1);
  assert(s1 == s2);

  return 0;
}

