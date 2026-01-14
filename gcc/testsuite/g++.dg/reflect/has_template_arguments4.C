// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_template_arguments.

#include <meta>

using namespace std::meta;

struct S {
  template<typename T>
  void fn (T) { }
};

template<typename T>
struct TS {
  template<typename U>
  void fn (T) { }
};

static_assert (!has_template_arguments (^^S::fn));
static_assert (has_template_arguments (^^S::fn<int>));
static_assert (!has_template_arguments (^^TS<int>::fn));
static_assert (has_template_arguments (^^TS<int>::fn<int>));
