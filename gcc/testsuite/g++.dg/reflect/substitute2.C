// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

template <info R>
struct A {
  static constexpr auto value = R;
};

template <class... R>
struct B { };

template <size_t I, typename J>
using C = [: template_arguments_of (^^J)[I] :];

template <class T>
using D = [: [] {
  std::vector<info> args;
  for (info b : bases_of (T::value, access_context::unchecked ()))
    args.push_back (substitute (^^A, { reflect_constant (b) }));
  return substitute (^^B, args); } () :];

struct E { };
struct F : E { };

constexpr auto b = bases_of (^^F, access_context::unchecked ())[0];
static_assert (std::same_as <C <0, D <A <^^F>>>, A <b>>);
