// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::parameters_of.

#include <meta>

template<auto R> void foo () {}  // { dg-message "sorry, unimplemented: mangling" }
void
g ()
{
  foo<std::meta::parameters_of(^^g)[0]>();
}
