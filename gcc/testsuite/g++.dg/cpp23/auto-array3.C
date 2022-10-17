// PR c++/102414
// PR c++/101874
// { dg-do compile { target c++11 } }
// { dg-options "" }

constexpr int sz () { return 3; }

void f ()
{
  int a[3];
  const int N = 3;
  auto (*a2)[N] = &a;
  constexpr int M = 3;
  auto (*a3)[M] = &a;
  auto (*a4)[sz()] = &a;
}
