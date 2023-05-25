// PR c++/109876
// { dg-do compile { target c++17 } }

struct Foo {};
template <const Foo&> struct X {};

void g()
{
  static constexpr Foo foo{};
  X<foo> x;
}

template<int>
void f()
{
  static constexpr Foo foo{};
  X<foo> x;
}

void
h ()
{
  f<0>();
  f<1>();
}
