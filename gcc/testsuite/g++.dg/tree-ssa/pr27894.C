// PR c++/27894
// { dg-do compile }
// { dg-options "-O" }

class A;
struct B
{
  B (unsigned long);
  int b2 () const;
  A *b1 () const;
};

enum { P = 0 };
enum O { Q = 75, };
class C;
struct D { A *d; };
struct E
{
  B e1 (int) const;
  A *e2 (const B &) const;
  D e3[4096];
};

inline A *
E::e2 (const B & x) const
{
  const D *w = &e3[x.b2 ()];
  return (A *) w->d;
}

extern E *e;

inline A *
B::b1 () const
{
  extern E *e;
  return e->e2 (*this);
}

template <class T> struct F : public B
{
  F (const B &);
  T *b1 () const;
};

template < class T > inline T * F <T>::b1 () const
{
  return (T *) B::b1 ();
};

typedef F <C> N;

class G {};
class H : public G {};
class I : public H {};
class J {};
class K {};
struct L
{
  void l (J *, C *, int, const char *, O);
};
class M : public K, public I
{
  void m (J &, int, const char *);
  void m (J &, int, int, const char *, float);
};

void
M::m (J &x, int y, const char *z)
{
  L *w = new L;
  N v = e->e1 (y);
  w->l (&x, v.b1 (), P, z, Q);
}

void
M::m (J &x, int y, int s, const char *z, float t)
{
  L *w = new L;
  N v = e->e1 (y);
  w->l (&x, v.b1 (), s, z, (O) (int) ((t) ? (50 + 20 / (float) t) : 0));
}
