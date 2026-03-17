// PR c++/123143
// { dg-do compile { target c++14 } }

constexpr int x = 42;
struct S { static constexpr int x = 20; template <int N> static constexpr int a = N; };

template<typename>
void
f ()
{
  constexpr S s;
  static_assert (s.template a<42> == 42, "");
  static_assert (s.S::template a<42> == 42, "");
}

void
g ()
{
  f<int>();
}
