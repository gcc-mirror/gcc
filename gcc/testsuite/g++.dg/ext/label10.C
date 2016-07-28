// PR c++/33836
// { dg-do compile }
// { dg-options "" }

template<int N> struct A
{
  enum { M = && N };	// { dg-error "referenced outside|cannot appear in|integral" }
};

A<0> a;

void foo ()
{
  __label__ P;
  enum { O = && P };	// { dg-error "cannot appear in|integral" }
  P:;
}
