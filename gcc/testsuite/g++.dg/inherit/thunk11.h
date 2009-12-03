struct A
{
  A () {}
  virtual ~A () {}
};
struct B
{
  B () {}
  virtual ~B () {}
};
struct C : public A, public B
{
  virtual void foo ();
  virtual ~C () {};
};
inline void C::foo () {}
