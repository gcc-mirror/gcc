// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_template_arguments.

#include <meta>
#include <vector>

using namespace std::meta;

struct S {
  template<typename T>
  struct N { };
};

template<typename T>
struct TS {
  template<typename U>
  struct N { };
};

static_assert (!has_template_arguments (^^S::N));
static_assert (has_template_arguments (^^S::N<int>));
static_assert (!has_template_arguments (^^TS<int>::N));
static_assert (has_template_arguments (^^TS<int>::N<int>));

template<typename T>
struct A { };

template<typename T>
struct A<T*> { };

template<>
struct A<int> { };

static_assert (!has_template_arguments (^^A));
static_assert (has_template_arguments (^^A<int>));
static_assert (has_template_arguments (^^A<int *>));

template<typename = int>
struct B { };
static_assert (has_template_arguments (^^B<>));

struct C {
  using Alias = A<int>;
};

static_assert (has_template_arguments (dealias (^^C::Alias)));
static_assert (!has_template_arguments (^^C::Alias));
