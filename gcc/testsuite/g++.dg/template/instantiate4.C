// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/10682: Typedef to enum template instantiation logic.

template <typename T>
struct Foo {
  enum E {a,b,c};
  typedef E EE;
};

void Baz(Foo<int>::EE x);
