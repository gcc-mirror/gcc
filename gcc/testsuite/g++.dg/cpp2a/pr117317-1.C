// PR c++/117317
// { dg-do compile { target c++20 } }

struct C {
  constexpr bool operator== (const C &b) const { return foo (); }
  constexpr virtual bool foo () const = 0;
};
class A : public C {};
class B : public C {};
template <int>
struct D : A, B
{
  constexpr bool operator== (const D &) const = default;
  constexpr bool foo () const override { return true; }
};
struct E : D<1> {};
constexpr E e;
constexpr E f;
static_assert (e == f, "");
