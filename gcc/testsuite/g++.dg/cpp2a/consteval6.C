// { dg-do compile }
// { dg-options "-std=c++2a" }

struct A {
  constexpr A () {}
  A (A const&) = delete;	// { dg-message "declared here" }
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
  T u = t;	// { dg-error "use of deleted function" }
}

using B = decltype (foo<A> ());
using C = decltype (bar<A> ());	// { dg-message "required from here" }
