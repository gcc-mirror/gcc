// Copyright (C) 2002 Free Software Foundation
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

struct A {
  virtual void f(const A* a) = 0;
};

struct B : virtual A {
  virtual void f(const A* a);
};

void B::f(const A* a)
{
  static_cast<const B&>(*a);             // { dg-error "" }
}
