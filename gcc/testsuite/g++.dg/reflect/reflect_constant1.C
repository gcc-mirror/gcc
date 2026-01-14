// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>

using namespace std::meta;
template<auto D>
  struct A { };

struct N { int x; };
struct K { char const* p; };


constexpr info r1 = reflect_constant(42);
static_assert(is_value(r1));
static_assert(r1 == template_arguments_of(^^A<42>)[0]);

constexpr info r2 = reflect_constant(N{42});
static_assert(is_object(r2));
static_assert(r2 == template_arguments_of(^^A<N{42}>)[0]);

constexpr info r3 = reflect_constant(K{nullptr});   // OK
static_assert(is_object(r3));
constexpr info r4 = reflect_constant(K{"ebab"});    // { dg-error "uncaught exception" }
