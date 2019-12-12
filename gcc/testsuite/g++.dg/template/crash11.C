// { dg-do compile }

// Origin: kparz@iastate.edu

// PR c++/7939: ICE for invalid function parameter after template
// substitution.

template <class T, class U> void foo(T, U) {}
template <class T> void foo<T,void>(T, void) {} // { dg-error "40:invalid use of type .void." }
// { dg-error "25:non-class, non-variable partial specialization" "" { target c++14 } .-1 }
// { dg-error "25:non-type partial specialization" "" { target c++11_down } .-2 }
