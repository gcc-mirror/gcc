// { dg-do compile }
// Origin: Wolfgang Bangerth <wolfgang.bangerth@iwr.uni-heidelberg.de>

// PR c++/2863
// Default function argument and template specialization.

struct X {
  template <int dim> void f(int=0);
};

template <> void X::f<1> () {}	// { dg-error "(not match|declaration)" }
