// PR c++/124306
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template <typename T>
void
foo (int a)
{
  constexpr auto b = parameters_of (^^foo <int>)[0];
  constexpr auto c = identifier_of (type_of (b));	// { dg-error "uncaught exception of type 'std::meta::exception'; 'what\\\(\\\)': 'reflection with has_identifier false'" }
}

void
bar (int a)
{
  foo <int> (a);
}
