// PR debug/48967
// { dg-do compile }
// { dg-options "-g -O2" }

template <typename> struct A;
template <typename T> struct A <T *>
{
  typedef T ref;
};
template <typename T, typename> struct B
{
  typedef A <T> t;
  typedef typename t::ref ref;
  ref operator * () { return ref (); }
};
template <typename T> struct I
{
  typedef T *cp;
  template <typename T1> struct J
  {
    typedef I <T1> other;
  };
};
template <typename T> struct S : public I <T>
{
};
template <typename T, typename _A> struct E
{
  typedef typename _A::template J <T>::other at;
};
template <typename T, typename _A = S <T> > struct D
{
  typedef E <T, _A> _Base;
  typedef typename _Base::at at;
  typedef typename at::cp cp;
  typedef B <cp, D> H;
};
template <class T> struct F
{
  T *operator -> () { return __null; }
};
template <typename T> long
lfloor (T x)
{
  return static_cast <long>(x) - (x && x != static_cast <long>(x));
}
template <typename T> long
lround (T x)
{
  return lfloor (x - 0.5) + 1;
}
class M;
template <typename> class P;
typedef P <M> Q;
template <typename> struct P
{
  float x ();
};
struct CV
{
  Q c;
};
struct C
{
  void foo (const CV &) const;
  class O;
  typedef D <F <O> > R;
  R n;
};
struct S3
{
  S3 (int, int);
};
struct S2
{
  S3 sx, sy;
  S2 (int x = 0, int y = 0, int s = 0, int t = 0) : sx (x, y), sy (s, t) {}
};
template <typename> struct N
{
  int bar ();
};
struct C::O
{
  N <float> o;
  void foo (CV r, int)
  {
    Q c = r.c;
    float t = 0.5 * (o.bar ());
    S2 (lround (c.x ()), t);
  }
};
void
C::foo (const CV &w) const
{
  R::H m;
  (*m)->foo (w, 8);
}
