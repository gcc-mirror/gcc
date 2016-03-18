// PR c++/70205

struct A
{
protected:
  static void f ();
};
struct B : A { };
struct C : A { };
struct D : C, B { void a () { D::f (); } };
struct E : D { void b () { D::f (); } };
