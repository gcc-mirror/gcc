// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Mar 2005 <nathan@codesourcery.com>

// PR 20465
// Origin: Matthias Klose <doko@debian.org>
//	   Andrew Pinski <pinskia@gcc.gnu.org>

template <class _Ret, class _Tp>
void mem_fun_ref(_Ret (_Tp::*__f)());

struct A {
  double f();
};

void h ()
{
  mem_fun_ref(&A::f);
}

template <class T>
void f()
{
  mem_fun_ref(&A::f);
}

void g()
{
  f<int>();
}
