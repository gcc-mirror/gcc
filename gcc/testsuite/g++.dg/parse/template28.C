// PR c++/79056

template<class> struct A {};

template<class T> void foo(A<T>=A<T>()) {} // { dg-error "'>=' should be '> =' to terminate a template argument list" }
// { dg-error "expected" "" { target *-*-* } .-1 }

void bar()
{
  foo(A<int>());		// { dg-error "" }
}
