// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Gabriel Dos_Reis <Gabriel.Dos_Reis@sophia.inria.fr>

template <template <typename> class C, typename T> class foo {
  typedef typename C<T>::bar bar;
  foo(bar);
};

template <template <typename> class C, typename T> foo<C,T>::foo(bar)
{} // { dg-bogus "" } C<T> not a class
