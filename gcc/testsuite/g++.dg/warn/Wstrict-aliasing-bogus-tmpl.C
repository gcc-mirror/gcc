// PR c++/94951
// { dg-do compile }
// { dg-options "-O2 -Wall" }

struct A { int a; };
template <int N>
struct B : public A
{
  static B<N> foo () { B<N> t; t.a = 4; return t; }	// { dg-bogus "dereferencing type-punned pointer will break strict-aliasing rules" }
};

B<0> b = B<0>::foo ();
