// { dg-do compile }

// Origin: robertk@mathematik.uni-freiburg.de
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/14479: Template header check for enum

template <int dim>
struct X {
  enum { dimension = dim };
  template<int d> void bar ();
};

template <>
template <>
void X<0>::bar<0> () {}
