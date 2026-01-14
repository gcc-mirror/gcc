// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_object.

#include <meta>
using namespace std::meta;

void f();

template <void (&F)()>
consteval void tfn() { }

static_assert (!is_object (template_arguments_of (^^tfn<f>)[0]));
static_assert (!is_object (^^f));
static_assert (template_arguments_of (^^tfn<f>)[0] == ^^f);
