// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_consteval_only.

#include <meta>
using namespace std::meta;

struct A {
  info i;
};
struct B : A { };
struct C : virtual A { };
struct D : B { };
struct E { };
struct F : E {
  info i;
};
template<typename T>
struct G {
  T t;
  using size_type = int;
};
struct H {
  G<info>::size_type sz;
};
struct I {
  std::initializer_list<std::meta::info>::size_type sz;
};
template<typename T>
struct J : T { };

template<typename T>
using U = J<T>;

template<typename T>
struct K {
  J<T> j;
};

template<typename T>
struct L {
  U<T> u;
};

template<typename T>
struct M : J<T> { };

template<typename>
struct N { };

static_assert (is_consteval_only_type (^^A));
static_assert (is_consteval_only_type (^^B));
static_assert (is_consteval_only_type (^^C));
static_assert (is_consteval_only_type (^^D));
static_assert (!is_consteval_only_type (^^E));
static_assert (is_consteval_only_type (^^F));
static_assert (!is_consteval_only_type (^^G<int>));
static_assert (is_consteval_only_type (^^G<info>));
static_assert (!is_consteval_only_type (^^H));
static_assert (!is_consteval_only_type (^^I));
static_assert (!is_consteval_only_type (^^J<E>));
static_assert (is_consteval_only_type (^^J<A>));
static_assert (!is_consteval_only_type (^^U<E>));
static_assert (is_consteval_only_type (^^U<A>));
static_assert (!is_consteval_only_type (^^K<E>));
static_assert (is_consteval_only_type (^^K<A>));
static_assert (!is_consteval_only_type (^^L<E>));
static_assert (is_consteval_only_type (^^L<A>));
static_assert (!is_consteval_only_type (^^M<E>));
static_assert (is_consteval_only_type (^^M<A>));
static_assert (!is_consteval_only_type (^^N<E>));
static_assert (!is_consteval_only_type (^^N<A>));

static_assert (is_consteval_only_type (^^std::meta::exception));
static_assert (is_consteval_only_type (^^std::meta::access_context));
static_assert (!is_consteval_only_type (^^std::meta::member_offset));
static_assert (is_consteval_only_type (^^std::meta::data_member_options));
static_assert (is_consteval_only_type (type_of (^^std::meta::data_member_options::name)));

struct O;
static_assert (!is_consteval_only_type (^^O));
struct O { info i; };
static_assert (is_consteval_only_type (^^O));
