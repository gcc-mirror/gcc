// PR debug/52048
// { dg-do compile }
// { dg-options "-fcompare-debug -fnon-call-exceptions -fno-tree-dominator-opts -O2" }
// { dg-additional-options "-Wno-return-type" }

template <typename T> struct A;
template <typename T>
struct A <T *>
{
  typedef T &a;
};
template <typename T>
struct B
{
  typedef typename A <T>::a a;
  a operator *() {}
};
template <typename T, typename U>
bool operator != (B <T>, B <U>)
{
}
template <typename T>
struct C
{
  typedef T *c;
  template <typename>
  struct D { typedef C d; };
};
template <typename T, typename U> struct E
{
  typedef typename U::template D <T>::d e;
};
template <typename T, typename U = C <T> >
struct F
{
  typedef E <T, U> b;
  typedef typename b::e e;
  typedef typename e::c c;
  typedef B <c> i;
  i begin ();
  i end ();
};
class G;
class H;
struct I
{
  void *i;
  template <typename T> T foo ();
};
struct J : public I
{
  virtual bool bar ();
};
class K {};
struct L
{
  bool baz () {}
};
struct M
{
  K m1 (K);
  K m2 (H *) {}
};
struct N : J
{
  bool bar (G &);
};
bool
N::bar (G &)
{
  M m = foo <M> ();
  F <H *> f;
  for (F <H *>::i I = f.begin (), E = f.end (); I != E;)
    {
      H *h = *I;
      L l;
      if (l.baz ())
	m.m1 (m.m2 (h));
    }
}
