// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct B { consteval virtual int fn() const { return 1; } };
struct D : B { consteval int fn() const override { return 2; } };

constexpr D d;
static_assert(d.[:^^D::fn:]() == 2);
static_assert(d.D::fn() == 2);
static_assert(d.[:^^B::fn:]() == 2);
static_assert(d.[:^^B:]::fn() == 1);
static_assert(d.B::fn() == 1);

// Splicing member as intermediate component of a member-access expression.
struct T { struct Inner { int v; } inner; };
constexpr auto r_inner = ^^T::inner;
constexpr T t = {{4}};
static_assert(t.[:r_inner:].v == 4);
