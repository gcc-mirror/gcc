// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by redleaf <e1wwater@dingo.cc.uq.edu.au>

struct B {
  template <class> void bar();
} b;

template <class T> void foo() {
  b.bar<T>(); // { dg-bogus "" } bar undeclared
  b.template bar<T>(); // { dg-bogus "" } ditto
  b.B::bar<T>();
}

template void foo<void>(); // { dg-bogus "" } 
