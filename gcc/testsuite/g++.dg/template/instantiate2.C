// { dg-do compile }
// Origin: Wolfgang Bangerth <wolfgang.bangerth@iwr.uni-heidelberg.de>

// PR c++/2862
// Default function argument and template instantiation.

template <int dim> void f (int=0) {};
template void f<1> ();		// { dg-error "not match" }
