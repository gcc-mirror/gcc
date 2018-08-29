// { dg-do compile }
// { dg-additional-options "-g" }

class A
{
public:
  virtual bool m_fn1 () const = 0;
};
class B
{
  const A *m_fn2 () const;
};
inline const A *
B::m_fn2 () const
{
  class C : A
  {
    bool
    m_fn1 () const
    {
      return true;
    }
    C () {}
  };

  return 0;
}
void
fn1 (A &p1)
{
  p1.m_fn1 ();
}
