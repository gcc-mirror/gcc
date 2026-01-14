// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct B { consteval char fn() const { return 'B'; } };
struct D : B { consteval char fn() const { return 'D'; } };

constexpr auto rBfn = ^^B::fn;
constexpr auto rDfn = ^^D::fn;

constexpr D d;
constexpr auto rd = ^^d;

static_assert([:rd:].[:rBfn:]() == 'B');
static_assert([:rd:].[:rDfn:]() == 'D');
