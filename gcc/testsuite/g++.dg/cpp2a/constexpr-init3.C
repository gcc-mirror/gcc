// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }

struct A
{
  int i;
  constexpr A() {} 
};

struct B
{
  A a;
};

// A::i not initialized.
constexpr B b[] = { {} }; // { dg-error "is not a constant expression" }
