// PR middle-end/6247
// This testcase was miscompiled on IA-32 because a single stack slot
// was used for 2 different variables at the same time.
// The function H::h1 was miscompiled.
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);
extern "C" void exit (int);

struct A
{
  A () { a = 1; }
  void a1 () { a++; }
  bool a2 () { return !--a; }
  unsigned int a;
};

struct B : public A
{
  B () : b (0) { a1 (); }
  void b1 ();
  const char *b;
};

struct C
{
  C ();
  C (const C &);
  ~C () { if (c->a2 ()) { if (c == c0) c0 = 0; c->b1 (); } }
  C &operator= (const C &);
  static C c1 (const char *x, int y = -1);
  C (int, bool);
  void a2 ();
  B *c;
  static B *c0;
};

B *C::c0 = __null;

template <class T> struct D
{
  D (const T& t) : d (t) {}
  D () {}
  D<T> *next, *prev;
  T d;
};

template<class T> struct E
{
  D<T> *e;
  E () : e (0) {}
  E (D<T> *p) : e (p) {}
  E (const E<T>& x) : e (x.e) {}
  const T& operator* () const { return e->d; }
  T& operator* () { return e->d; }
  bool operator== (const E<T>& x) const { return e == x.e; }
  bool operator!= (const E<T>& x) const { return e != x.e; }
  E<T> operator++ (int) { E<T> x = *this; e = e->next; return x; }
};

template <class T> struct F : public A
{
  F () { f = new D<T>; f->next = f->prev = f; f0 = 0; }
  ~F () {}
  D<T> *f;
  unsigned int f0;

  F (const F<T>& x) : A ()
  {
    f = new D<T>; f->next = f->prev = f; f0 = 0;
    E<T> b (x.f->next);
    E<T> e (x.f);
    E<T> i (f);
    while (b != e)
      f1 (i, *b++);
  }

  E<T> f1 (E<T> x, const T& y)
  {
    D<T> *p = new D<T> (y);
    p->next = x.e;
    p->prev = x.e->prev;
    x.e->prev->next = p;
    x.e->prev = p;
    f0++;
    return p;
  }
};

template <class T> struct G
{
  E<T> g1 () { g3 (); return E<T> (g->f); }
  E<T> g2 (const T& x) { g3 (); return g->f1 (g1 (), x); }
  void g3 () { if (g->a > 1) { g->a2 (); g = new F<T> (*g); } }
  F<T>* g;
};

struct H
{
  virtual ~H () {};
  virtual void h1 ();
  struct I
  {
    I () {}
    I (C r, C p) : i1 (r), i2 (p) {}
    C i1, i2;
  };
  G<I> h;
};

void H::h1 ()
{
  h.g2 (I (C::c1 ("s1"), C::c1 ("t")));
  h.g2 (I (C::c1 ("s2"), C::c1 ("t")));
  h.g2 (I (C::c1 ("s3"), C::c1 ("t")));
}

void B::b1 ()
{
}

C C::c1 (const char *x, int y)
{
  C z;

  if (y != -1)
    abort ();
  z.c = new B;
  z.c->b = x;
  return z;
}

C::C () : c (__null)
{
}

C::C (const C &x)
{
  c = x.c;
  c->a1 ();
}

int main ()
{
  H h;
  h.h.g = new F<H::I> ();
  h.h1 ();
  if (h.h.g->f0 != 3)
    abort ();
  D<H::I> *p;
  int i;
  for (i = 0, p = h.h.g->f; i < 4; i++, p = p->next)
    {
      if (i == 0 && (p->d.i1.c != __null || p->d.i2.c != __null))
	abort ();
      if (i > 0
	  && (p->d.i1.c->b[0] != 's'
	      || p->d.i1.c->b[1] != '0' + i
	      || p->d.i1.c->b[2] != '\0'
	      || __builtin_strcmp (p->d.i2.c->b, "t")))
	abort ();
      if (p->prev->next != p)
	abort ();
      if (p->next->prev != p)
	abort ();
      if (i == 3 && p->next != h.h.g->f)
	abort ();
    }
  exit (0);
}
