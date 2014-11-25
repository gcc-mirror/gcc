// { dg-options "-r -nostdlib -O2 -flto -fno-devirtualize" }
// { dg-require-effective-target lto }

class A;
class B
{
public:
  A *operator->();
};
class C
{
public:
  virtual void m_fn1 ();
};
class A
{
public:
  C *m_fn2 ();
};
class D
{
public:
  void
  m_fn3 ()
  {
    list_m->m_fn2 ()->m_fn1 ();
  }
  B list_m;
};

class F
{
public:
  D m_fn4 ();
};
class G
{
public:
  F m_fn5 (int, int);
};
class H
{
public:
  void
  m_fn6 ()
  {
    fieldEngine_m.m_fn5 (0, 0).m_fn4 ().m_fn3 ();
  }
  G fieldEngine_m;
};

void
fn1 (H a)
{
  a.m_fn6 ();
}
