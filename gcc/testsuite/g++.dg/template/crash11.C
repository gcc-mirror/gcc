// { dg-do compile }

// Origin: kparz@iastate.edu

// PR c++/7939: ICE for invalid function parameter after template
// substitution.

template <class T, class U> void foo(T, U) {}
template <class T> void foo<T,void>(T, void) {} // { dg-error "incomplete|invalid|partial" }
