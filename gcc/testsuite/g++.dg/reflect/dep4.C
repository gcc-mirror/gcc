// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

consteval int fn1 () { return 42; }
consteval int fn2 (int a = 5) { return a; }

constexpr info r1 = ^^fn1;
constexpr info r2 = ^^fn2;
static_assert([: r1 :]() == 42);
static_assert([: r2 :]() == 5);
static_assert([: r2 :](11) == 11);

// With a dependent reflection.
template <info R> consteval int fn() { return [:R:](); }
static_assert(fn<r1>() == 42);
static_assert(fn<r2>() == 5);

void
runtime ()
{
  [:^^runtime:];
}
