// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::template_arguments_of.

#include <meta>

using namespace std::meta;

using U = int;
typedef double V;

template<typename T, typename U>
struct S {};

static_assert (template_arguments_of (^^S<int, double>)[0] == ^^int);
static_assert (template_arguments_of (^^S<int, double>)[1] == ^^double);
static_assert (template_arguments_of (^^S<U, V>)[0] == ^^int);
static_assert (template_arguments_of (^^S<U, V>)[1] == ^^double);
