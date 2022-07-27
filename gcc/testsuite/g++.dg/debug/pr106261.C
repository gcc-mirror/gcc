// PR debug/106261
// { dg-do compile }
// { dg-options "-dx -fno-dwarf2-cfi-asm" }

struct A
{
  virtual void foo ();
  int a;
};
class C : virtual public A
{
};
struct B
{
  A *b;

  B (A *x) : b (x) { b->foo (); }
};
struct E
{
  virtual ~E ();
};
class D : public C, E
{
};
struct F : D
{
  F (int);

  static void bar ()
  {
    F a (0);
    B b (&a);
  }
};
void baz () { F::bar (); }
