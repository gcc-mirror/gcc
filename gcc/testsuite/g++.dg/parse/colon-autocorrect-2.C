// PR c++/44523
// { dg-do compile }

namespace x {
  struct a { };
}

template <typename t>
class foo {
};

foo<x::a> a1;
foo<x:a> a2;			// { dg-error "nested-name-specifier" }

x::a a3 = a2;			// { dg-error "conversion" }
