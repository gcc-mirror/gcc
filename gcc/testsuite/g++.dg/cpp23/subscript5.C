// P2128R6
// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include <initializer_list>
#include <cstdlib>

struct S
{
  S () : a {} {};
  int &operator[] (std::initializer_list<int> l) {
    int sum = 0;
    for (auto x : l)
      sum += x;
    return a[sum];
  }
  int a[64];
};

int
main ()
{
  S s;
  if (&s[{0}] != &s.a[0]
      || &s[{42}] != &s.a[42]
      || &s[{5, 7, 9}] != &s.a[5 + 7 + 9]
      || &s[{1, 2, 3, 4}] != &s.a[1 + 2 + 3 + 4])
    abort ();
}
