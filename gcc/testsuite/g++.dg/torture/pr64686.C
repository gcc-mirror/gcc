// { dg-do compile }
class A
{
protected:
  A *m_fn2 (int) const;
public:
  virtual A *m_fn1 (int *) const = 0;
};
class B : A
{
  B (A *, int, A *);
  A *m_fn1 (int *) const;
};
A *
B::m_fn1 (int *) const
{
  new B (m_fn2 (0)->m_fn1 (0), 0, m_fn2 (0)->m_fn1 (0));
}

