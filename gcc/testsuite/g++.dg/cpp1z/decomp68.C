// PR c++/120285
// { dg-do compile { target c++17 } }

struct S {
  int a = 1;
};

void
g ()
{
  S arr[2];
  auto [a, b] = arr;
  auto [c, d](arr);
  S arr2[2][2];
  auto [e, f] = arr2;
  auto [g, h](arr2);
  S arr3[2][2];
  auto [i, j] = arr3;
  auto [k, l](arr3);
}
