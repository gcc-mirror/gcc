// PR c++/117317
// { dg-do compile { target c++20 } }

struct C {
  constexpr virtual bool foo () const = 0;
};
struct A : public C {};
struct B : public C {};
template <int>
struct D : A, B
{
  constexpr bool foo () const override { return true; }
};
constexpr D<0> d;
static_assert (d.foo (), "");
