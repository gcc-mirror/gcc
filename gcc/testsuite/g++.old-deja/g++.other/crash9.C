// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

struct A { };
struct B : public A
{
  int A;
};
struct C : public B { };
