// { dg-options "-Wtemplates -Wmultiple-inheritance -Wvirtual-inheritance -Wnamespaces" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#define _GLIBCXX_SYSHDR
#include <iostream>
#include <algorithm>

namespace foo { } // { dg-warning "namespace" }

template <typename X> X Foo (); // { dg-warning "template" }

struct B1 {};
struct B2 {};
struct V {};

struct D :  B1, B2 {}; //  { dg-warning "multiple" }

struct E : virtual V {};  // { dg-warning "virtual" }

struct F1 : E {};

struct F2 : D {};

void Baz (int a, int b)
{
  std::swap (a, b);
}


