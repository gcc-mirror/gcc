// PR c++/70147
// { dg-do run }
// { dg-options "-fsanitize=vptr" }

struct A { A () {} virtual void f () {} };
struct B : virtual A { B () {} virtual void f () {} };
struct C : B, virtual A { C () {} } c;

int
main ()
{
}
