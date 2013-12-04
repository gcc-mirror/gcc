// PR c++/58874
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A
{
  template<int> struct B
  {
    #pragma omp declare reduction (x : int : omp_out |= omp_in)
  };
};

#pragma omp declare reduction (y : long : omp_out |= omp_in) \
	initializer (omp_priv = 0)
