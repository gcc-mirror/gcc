// PR c++/31598
// { dg-do compile }
//
// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Theodore.Papadopoulo
//   16 Apr 2007 <Theodore.Papadopoulo@sophia.inria.fr>

int i;
template <typename> struct A { A() {} };
template <typename> struct C { C() { i++; } C(const C &) { i += 2; } };
struct D { D() {} };

struct M { typedef double E; };

template <typename T>
struct R
{
  R()
  {
    typedef A<typename T::E> B;
    B b;
    #pragma omp parallel for firstprivate(b) schedule(guided)
      for (int t = 0; t < 10; ++t)
	;
  }
};

template <typename T>
struct S
{
  S()
  {
    typedef C<typename T::E> B;
    B b;
    #pragma omp parallel for firstprivate(b)
      for (int t = 0; t < 10; ++t)
	;
  }
};

struct U
{
  U()
  {
    D b;
    #pragma omp parallel for firstprivate(b)
      for (int t = 0; t < 10; ++t)
	;
  }
};

int
main ()
{
  R<M> r;
  S<M> s;
  U u;
  return 0;
}
