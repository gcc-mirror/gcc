// PR c++/89119
// { dg-do compile { target c++11 } }

struct S { int a[4]; };

template<int N>
struct R { int a[N]; };

template <typename T>
void
fn ()
{
  constexpr auto s = S();
  constexpr auto s2 = S{};
  constexpr auto r = R<4>();
  constexpr auto r2 = R<4>{};
}

void
foo ()
{
  fn<int>();
}
