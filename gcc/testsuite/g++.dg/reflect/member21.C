// PR c++/124331
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct B { consteval virtual int fn() const { return 1; } };
struct D : B { consteval int fn() const override { return 2; } };

constexpr D d;

template<typename T, typename U>
void
f ()
{
  static_assert(d.[:^^U::fn:]() == 2);
  static_assert(d.[:^^D::fn:]() == 2);
  static_assert(d.U::fn() == 2);
  static_assert(d.D::fn() == 2);
  static_assert(d.fn() == 2);
  static_assert(d.[:^^T::fn:]() == 2);
  static_assert(d.[:^^T:]::fn() == 1);
  static_assert(d.T::fn() == 1);
  static_assert(d.[:^^B::fn:]() == 2);
  static_assert(d.[:^^B:]::fn() == 1);
  static_assert(d.B::fn() == 1);
}

void
g ()
{
  f<B, D>();
}
