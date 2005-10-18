// PR c++/22293
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

struct A
{
  friend ~A();  // { dg-error "qualified name" }
};

struct B
{
  friend ~A();  // { dg-error "qualified name" }
};
