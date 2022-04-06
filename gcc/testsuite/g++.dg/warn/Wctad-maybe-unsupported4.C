// PR c++/105143
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Werror=ctad-maybe-unsupported" }

template<class...> struct A { };

template<template<class...> class TT> auto f(...) -> decltype(TT()); // #1
template<template<class...> class TT> void f(int); // #2

int main() {
  f<A>(0); // Calls #2 without issuing a -Wctad-maybe-unsupported
	   // diagnostic for #1.
}
