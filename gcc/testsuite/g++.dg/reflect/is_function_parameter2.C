// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_function_parameter.

#include <meta>

using namespace std::meta;

void fn (int a, bool &b, std::string *, ...);

static_assert (is_function_parameter (parameters_of(^^fn)[0]));
static_assert (is_function_parameter (parameters_of(^^fn)[1]));
static_assert (is_function_parameter (parameters_of(^^fn)[2]));
