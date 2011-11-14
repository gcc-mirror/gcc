// { dg-do compile }

struct A { typedef int type; };
struct B { typedef int type; };
struct C : B { using B::type; };

struct D : A, C
{
  using C::type;
  void f() { type t = 0;}
};
