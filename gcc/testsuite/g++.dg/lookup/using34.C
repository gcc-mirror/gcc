// { dg-do compile }

struct A { int f (); };
struct B : A
{
  using A::f;
  struct f {};
  void g() { f(); struct f ff; }
  struct f ff;
};
