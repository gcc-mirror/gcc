/* { dg-do compile } */
struct A
{
  virtual void m_fn1 ();
  int m_local;
};
class C : virtual public A
{
};
struct B
{
  A *m_g;

  B (A *p1) : m_g (p1) { m_g->m_fn1 (); }
};
struct C7
{
  virtual ~C7 ();
};
class D : public C, C7
{
};
struct F : D
{
  F (int);

  static void m_fn2 ()
  {
    F a (0);
    B b (&a);
  }
};
void fn1 () { F::m_fn2 (); }
