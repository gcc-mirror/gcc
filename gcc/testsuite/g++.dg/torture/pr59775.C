// { dg-do compile }
struct A
{
  virtual void foo () = 0;
  void bar () { foo (); }
  bool a;
};
struct B : public virtual A
{
  virtual void foo ();
};
struct C : public B
{
  C ();
};
void
baz ()
{
  C c;
  c.bar ();
}
