// { dg-do compile }

// Avoid -pedantic-error default
// { dg-options "" }
 
// PR 19989 - dependent array of size 0 fails to compile.

template<int I> struct A
{
  static const int zero = 0;
};

template<int N> struct B
{
  int x[A<N>::zero];
};

B<0> b;
