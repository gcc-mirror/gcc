// PR c++/50243
// { dg-do compile }
// { dg-options "-O" }
// { dg-final { scan-assembler-not "_ZTV.A" } }

void foo ();

struct A
{
  ~A () { }
  virtual void a () = 0;
  virtual void b () = 0;
  virtual void c () = 0;
};

struct B : public A
{
  ~B () { foo (); }
  void a () { foo (); }
  void b () { foo (); }
  void c () { delete this; }
};

void
test ()
{
  A *y = new B ();
  y->a ();
  y->b ();
  y->c ();
}
