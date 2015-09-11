// { dg-do compile }
// { dg-options "-O -fdeclone-ctor-dtor" }
struct A {
  virtual ~ A ();
};

struct B : virtual A {};
struct C : virtual A {};

struct D : B, C {};

D d;
