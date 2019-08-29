// PR c++/79056

template<class> struct A {};

template<class T> void foo(A<T>=A<T>()) {} // { dg-error "24:variable or field .foo. declared void" }
// { dg-error "template" "" { target *-*-* } .-1 }

void bar()
{
  foo(A<int>());		// { dg-error "" }
}
