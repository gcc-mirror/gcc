// PR c++/27177

struct Z {};
struct A : Z {};

Z* implicitToZ (Z*);

struct B : A
{
  static const int i = sizeof(implicitToZ((B*)0));
};

