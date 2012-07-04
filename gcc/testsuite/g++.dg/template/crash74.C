// PR c++/34089
// { dg-do compile }
// { dg-options "" }

template<typename F> void foo () { } // { dg-prune-output "previous" }
template<typename F> struct foo<F> { };	// { dg-error "template" }
