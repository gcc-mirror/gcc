// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Aug 2000 <nathan@codesourcery.com>

// bug 371 We failed to subst explicit template args before trying to
// deduce the template.

namespace N
{
enum E { e0 };

template< E e > void nf();

}

template< N::E e > void gf();

struct X {
  template<N::E e> void xfn ();
  template<N::E e> static void sfn ();
};

template < class C >
void tf(C *ptr)
{
  N::nf<N::e0>();
  gf<N::e0>();
  ptr->X::xfn <N::e0> ();
  ptr->C::template xfn <N::e0> ();
  ptr->template xfn <N::e0> ();
  ptr->X::sfn <N::e0> ();
  ptr->C::template sfn <N::e0> ();
  ptr->template sfn <N::e0> ();
  X::sfn <N::e0> ();
  C::template sfn <N::e0> ();
}

void f(X *ptr)
{
  ptr->xfn <N::e0> ();
  tf(ptr);
}
