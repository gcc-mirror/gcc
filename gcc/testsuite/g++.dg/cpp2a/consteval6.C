// { dg-do compile }
// { dg-options "-std=c++2a" }

struct A {
  constexpr A () {}
  A (A const&) = delete;	// { dg-bogus "declared here" }
};

template<typename T>
constexpr void
foo ()
{
  T t;
  T u = t;
}

template<typename T>
consteval void
bar ()
{
  T t;
  T u = t;	// { dg-bogus "use of deleted function" }
}

using B = decltype (foo<A> ());
using C = decltype (bar<A> ());	// { dg-bogus "required from here" }
