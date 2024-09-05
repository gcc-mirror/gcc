// PR c++/102594
// { dg-do compile { target c++17 } }

struct S {
  S() {}
};
S arr1[2];
S arr2[2][1];
S arr3[1][1];
auto [m](arr3);
auto [n] = arr3;

struct X {
  int i;
};

void
g (X x)
{
  auto [a, b](arr2);
  auto [c, d] = arr2;
  auto [e, f] = (arr2);
  auto [i, j](arr1);
  auto [k, l] = arr1;
  auto [m, n] = (arr1);
  auto [z] = x;
  auto [y](x);
}
