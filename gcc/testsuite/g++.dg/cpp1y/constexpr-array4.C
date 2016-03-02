// { dg-do compile { target c++14 } }

constexpr bool g()
{
  int ar[4] = { 1, 2, 3, 4 };
  auto e1 = ar;
  auto e4 = ar+3;
  return (e4-e1) == 3;
}

#define SA(X) static_assert((X),#X)
SA(g());
