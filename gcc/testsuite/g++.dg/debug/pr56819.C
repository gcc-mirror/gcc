// PR debug/56819
// { dg-do compile }
// { dg-options "-fcompare-debug" }
// { dg-xfail-if "" { powerpc-ibm-aix* } }

template <typename>
struct A
{
  template <typename>
  struct B;
};

template <typename>
struct C
{
  typedef int I;
};

template <typename T>
class D
{
  typedef A <void> E;
  typedef typename T::template B <E> F;
  typedef typename C <F>::I I;
  A <I> foo () { return A<I> (); }
};

template class D <A <void> >;
