// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }

struct A
{
  int i;
  constexpr A() : i{} {} 
};

struct B
{
  A a;
};

constexpr B b[] = { {} };
