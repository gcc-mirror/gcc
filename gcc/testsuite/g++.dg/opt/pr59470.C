// PR middle-end/59470
// { dg-do run }
// { dg-options "-O2 -fstack-protector" }
// { dg-additional-options "-fPIC" { target fpic } }
// { dg-require-effective-target fstack_protector }

struct A
{
  int a1;
  A () throw () : a1 (0) {}
};

struct B
{
  unsigned int b1 () throw ();
};

__attribute__((noinline, noclone)) unsigned int
B::b1 () throw ()
{
  asm volatile ("" : : : "memory");
  return 0;
}

struct C
{
  const A **c1;
  void c2 (const A *, unsigned int);
};

__attribute__((noinline, noclone)) void
C::c2 (const A *, unsigned int)
{
  asm volatile ("" : : : "memory");
}

struct D
{
  C *d1;
};

struct E
{
  int e1;
  int e2;
  D e3;
};

struct F
{
  virtual int f1 (const char * s, int n);
};

struct G
{
  F *g1;
  bool g2;
  G & g3 (const char * ws, int len)
  {
    if (__builtin_expect (!g2, true)
	&& __builtin_expect (this->g1->f1 (ws, len) != len, false))
      g2 = true;
    return *this;
  }
};

struct H : public A
{
  const char *h1;
  unsigned int h2;
  bool h3;
  const char *h4;
  char h5;
  char h6;
  char h7[31];
  bool h8;
  H () : h1 (0), h2 (0), h4 (0), h5 (0), h6 (0), h8 (false) {}
  void h9 (const D &) __attribute__((noinline, noclone));
};

void
H::h9 (const D &)
{
  h3 = true;
  __builtin_memset (h7, 0, sizeof (h7));
  asm volatile ("" : : : "memory");
};

B b;

inline const H *
foo (const D &x)
{
  const unsigned int i = b.b1 ();
  const A **j = x.d1->c1;
  if (!j[i])
    {
      H *k = 0;
      try
	{
	  k = new H;
	  k->h9 (x);
	}
      catch (...)
	{
	}
      x.d1->c2 (k, i);
    }
    return static_cast <const H *>(j[i]);
}

__attribute__((noinline, noclone)) int
bar (char *x, unsigned long v, const char *y, int z, bool w)
{
  asm volatile ("" : : "r" (x), "r" (v), "r" (y) : "memory");
  asm volatile ("" : : "r" (z), "r" (w) : "memory");
  return 8;
}

__attribute__((noinline, noclone)) void
baz (void *z, const char *g, unsigned int h, char s, E &e, char *n, char *c, int &l)
{
  asm volatile ("" : : "r" (z), "r" (g), "r" (h) : "memory");
  asm volatile ("" : : "r" (s), "r" (&e), "r" (n) : "memory");
  asm volatile ("" : : "r" (c), "r" (&l) : "memory");
  if (n == c)
    __builtin_abort ();
  int i = 0;
  asm ("" : "+r" (i));
  if (i == 0)
    __builtin_exit (0);
}

__attribute__((noinline, noclone)) G
test (void *z, G s, E &x, char, long v)
{
  const D &d = x.e3;
  const H *h = foo (d);
  const char *q = h->h7;
  const int f = x.e2;
  const int i = 5 * sizeof (long);
  char *c = static_cast <char *>(__builtin_alloca (i));
  const int b = f & 74;
  const bool e = (b != 64 && b != 8);
  const unsigned long u = ((v > 0 || !e) ? (unsigned long) v : -(unsigned long) v);
  int l = bar (c + i, u, q, f, e);
  c += i - l;
  if (h->h3)
    {
      char *c2 = static_cast <char *>(__builtin_alloca ((l + 1) * 2));
      baz (z, h->h1, h->h2, h->h6, x, c2 + 2, c, l);
      c = c2 + 2;
    }
  if (__builtin_expect (e, true))
    {
    }
  else if ((f & 4096) && v)
    {
      {
	const bool m = f & 176;
	*--c = q[m];
	*--c = q[1];
      }
    }
  const int w = x.e1;
  if (w > l)
    {
      char * c3 = static_cast <char *>(__builtin_alloca (w));
      c = c3;
    }
  return s.g3 (c, l);
}

int
main ()
{
  H h;
  const A *j[1];
  C c;
  G g;
  E e;
  h.h9 (e.e3);
  j[0] = &h;
  c.c1 = j;
  e.e3.d1 = &c;
  test (0, g, e, 0, 0);
  __builtin_abort ();
}
