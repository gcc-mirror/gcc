/* PR ipa/91969 */
/* { dg-options "-fdump-ipa-inline -O3" } */

enum by
{
};
class A
{
public:
  class B
  {
  public:
    virtual void m_fn2 (by) = 0;
  };
  virtual int m_fn1 ();
  B *cf;
};
by a;
class C : A, A::B
{
  void m_fn2 (by);
};
void C::m_fn2 (by) { cf->m_fn2 (a); }

struct a
{
  virtual ~a ();
};

struct b
{
  virtual void d (...);
};

struct c : a, b
{
  void d (...) {}
};
