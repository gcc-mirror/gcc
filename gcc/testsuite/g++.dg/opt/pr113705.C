// PR middle-end/113705
// { dg-do compile { target c++17 } }
// { dg-options "-O2 -w" }

void foo ();
template <typename T> struct A : T { long bar () const; };
int a;

template <typename T>
long
A<T>::bar () const
{
  return this->baz ()[a];
}

struct B {
  struct { long b[1]; long c; } u;
  unsigned d;
  int e;
  B (const B &);
  ~B ();
  const long *baz () const;
  unsigned qux () const;
};

B::B (const B &)
{
  if (__builtin_expect (e, 0))
    u.c = 0;
}

B::~B ()
{
  if (__builtin_expect (e, 0))
    foo ();
}

const long *
B::baz () const
{
  return u.b;
}

unsigned
B::qux () const
{
  return d;
}

struct C { A<B> corge () const; A<B> *f; };

A<B>
C::corge () const
{
  return f[1];
}

void
test (C r, long *h, unsigned short *d)
{
  for (int j = 0; j < 8; ++j)
    {
      A g = r.corge ();
      *d = g.qux ();
      for (unsigned i = 0; i < *d; ++i)
	*h++ = g.bar ();
    }
}
