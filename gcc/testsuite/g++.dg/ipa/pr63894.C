/* { dg-do compile } */
/* { dg-options "-O3" } */

struct A
{
  void *operator new(__SIZE_TYPE__, int);
};
class C
{
public:
  C (int);
};
class D
{
public:
  enum Type
  {
    BOX
  };
  D (int, Type, C);
};
class F
{
public:
  virtual void m_fn1 (int, D);
};
class G : public F, public A
{
};
class K : public G
{
public:
  K (C, D);
};
class J
{
  D m_fn2 (int);
  bool m_fn3 (G *);
};
bool
J::m_fn3 (G *p1)
{
  p1->m_fn1 (0, D (0, D::BOX, 0));
  K *d = new (0) K (0, m_fn2 (0));
  m_fn3 (d);
  return true;
}
