// PR c++/26256
// { dg-do compile }

struct A { int next; };
struct B { int next; };
struct C : B { using B::next; };

struct D : A, C
{
   using C::next;
   void f() { next = 1; }
};
