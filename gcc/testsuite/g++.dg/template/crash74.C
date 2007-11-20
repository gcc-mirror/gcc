// PR c++/34089
// { dg-do compile }
// { dg-options "" }

template<typename F> void foo () { }
template<typename F> struct foo<F> { };	// { dg-error "redeclared as" }
