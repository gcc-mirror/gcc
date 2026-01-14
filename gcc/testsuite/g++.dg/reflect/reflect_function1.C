// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_function.

#include <meta>

using namespace std::meta;

struct S {
  void foo(this S) {}
};

int
main ()
{
  [:reflect_function(*&S::foo):](S()); // { dg-error "cannot implicitly reference" }
}
