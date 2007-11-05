// PR c++/33836
// { dg-do compile }
// { dg-options "-std=gnu++98" }

template<int N> struct A
{
  enum { M = && N };	// { dg-error "referenced outside|cannot appear in" }
};

A<0> a;

void foo ()
{
  __label__ P;
  enum { O = && P };	// { dg-error "cannot appear in" }
  P:;
}
