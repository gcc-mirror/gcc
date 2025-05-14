// { dg-do compile { target c++11 } }

struct S { void *p; };
struct T { S s; };
constexpr S s = { nullptr };
constexpr T t = { { nullptr } };

constexpr void *
foo ()
{
  return ((void **) &t)[0];	// { dg-error "reinterpret_cast" }
}

constexpr void *
bar ()
{
  return ((void **) &s)[0];	// { dg-error "reinterpret_cast" }
}

constexpr auto x = foo (); // { dg-error "called in a constant expression" }
constexpr auto y = bar (); // { dg-error "called in a constant expression" }
