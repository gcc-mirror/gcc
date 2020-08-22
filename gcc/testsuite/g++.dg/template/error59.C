template<int N> struct S { };

template<template<typename> class TT>
void foo();

void bar()
{
  foo<S>(); // { dg-error "no matching function" }
  // { dg-error "type/value mismatch at argument 1" "" { target *-*-* } .-1 }
  // { dg-message "expected a template of type .template<class> class TT., got .template<int N> struct S." "" { target *-*-* } .-2 }
}
