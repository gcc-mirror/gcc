// { dg-do compile }
// Origin: <togawa at acm dot arg>
// c++/2094: unsupported 'ptrmem_cst' in type unification

struct R
{
  int i;
};

struct S
{
  int i;
  int j;
};

struct S2 : S
{};

template<int S::*p, typename>
struct X
{
  X ();
  template<typename U> X(const X<p,U> &);
};

X<&S::i,S> x  = X<&S::i,S>();
X<&S::i,S> x2 = X<&S2::i,S>();
X<&S::i,S> y  = X<&S::j,S>();  // { dg-error "" }
X<&S::i,S> z  = X<&R::i,S>();  // { dg-error "" }
