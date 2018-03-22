// { dg-do compile }
template <class _T1> struct A
{
  _T1 first;
};
struct B
{
  int operator!=(B);
};
template <typename _Tp> struct C
{
  C (B);
  _Tp operator*();
  int operator!=(C);
};
template <typename _Tp> class D
{
public:
  typedef C<_Tp> const_iterator;
  const_iterator m_fn1 () const;
  B m_fn2 ();
  void m_fn3 ();
};
class F
{
  struct G
  {
    static G &
    m_fn5 ()
    {
      void fn1 ();
      return *reinterpret_cast<G *> (fn1);
    }
    int *
    m_fn6 ()
    {
      return reinterpret_cast<int *> (this);
    }
  };
  struct _Alloc_hider
  {
    _Alloc_hider (int *p1, int) : _M_p (p1) {}
    int *_M_p;
  } _M_dataplus;
  G &
  m_fn4 ()
  {
    return G::m_fn5 ();
  }
public:
  F () : _M_dataplus (m_fn4 ().m_fn6 (), 0) {}
};
class H
{
  void m_fn7 (const F &, bool &);
  void m_fn8 (const D<F> &, const F &, F &);
};
typedef A<int> CandPair;
class I
{
public:
  virtual void m_fn9 (const F &, bool, D<CandPair> &);
};
class J : I
{
public:
  void m_fn9 (const F &, bool, D<CandPair> &);
};
D<I *> c;
void
J::m_fn9 (const F &, bool, D<CandPair> &)
{
  D<int> a;
  for (B b; b != a.m_fn2 ();)
    ;
}
inline void
fn2 (F p1, int, int, J *p4, D<CandPair>)
{
  D<CandPair> d;
  d.m_fn3 ();
  p4->m_fn9 (p1, 0, d);
  for (D<I *>::const_iterator e = c.m_fn1 (); e != c.m_fn2 ();)
    (*e)->m_fn9 (p1, 0, d);
}
void
H::m_fn7 (const F &, bool &)
{
  A<F> f;
  D<F> g;
  F h;
  m_fn8 (g, f.first, h);
}
void
H::m_fn8 (const D<F> &p1, const F &, F &)
{
  F i;
  p1.m_fn1 ();
  D<CandPair> j;
  fn2 (i, 0, 0, 0, j);
}
