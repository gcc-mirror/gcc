// This testcase was miscompiled on IA-64 to read from unitialized memory
// and dereference it.
// { dg-do run }
// { dg-options "-O2" }

struct A
{
  A () { a = 1; }
  void a1 () { a++; }
  bool a2 () { return !--a; }
  unsigned int a;
};

struct B {};

template <class T> struct C
{
  C () {}
  C (const T& t) : c (t) {}
  C<T> *next, *prev;
  T c;
};

template <class T> struct D
{
  C<T> *d;
  D () : d (0) {}
  D (C<T> *x) : d (x) {}
  D (const D<T>& x) : d (x.d) {}
  bool operator!= (const D<T>& x) const { return d != x.d; }
  const T& operator* () const { return d->c; }
  D<T> operator++ (int) { D<T> t = *this; d = d->next; return t; }
};

template <class T> struct E
{
  C<T> *e;
  E () : e (0) {}
  E (C<T> *p) : e (p) {}
  E (const E<T>& x) : e (x.e) {}
  E (const D<T>& x) : e (x.e) {}
  bool operator!= (const E<T>& x) const { return e != x.e; }
  const T& operator* () const { return e->c; }
  E<T>& operator++ () { e = e->next; return *this; }
};

template <class T> struct F : public A
{
  C<T> *f;
  unsigned long f0;
  F () { f = new C<T>; f->next = f->prev = f; f0 = 0; }
  F (const F<T>& x) : A ()
  {
    f = new C<T>; f->next = f->prev = f; f0 = 0;
    D<T> b (x.f->next), e (x.f), i (f);
    while (b != e)
      f1 (i, *b++);
  }

  ~F ()
  {
    C<T> *p = f->next;
    while (p != f)
      {
	C<T> *x = p->next;
	delete p;
	p = x;
      }
    delete f;
  }

  D<T> f1 (D<T> x, const T& y)
  {
    C<T> *p = new C<T> (y);
    p->next = x.d;
    p->prev = x.d->prev;
    x.d->prev->next = p;
    x.d->prev = p;
    f0++;
    return p;
  }
};

template <class T> struct G
{
  F<T> *g;
  G () { g = new F<T>; }
  G (const G<T>& x) { g = x.g; g->a1 (); }
  ~G () {}
  G<T>& operator= (const G<T>& x) { x.g->a1 (); g = x.g; return *this; }
  D<T> g1 () { g4 (); return D<T> (g->f); }
  E<T> g1 () const { return E<T> (g->f); }
  E<T> g2 () const { return E<T> (g->f->next); }
  D<T> g3 (const T& x) { g4 (); return g->f1 (g1 (), x); }
  void g4 () { if (g->a > 1) { g->a2 (); g = new F<T> (*g); } }

  G<T> operator+ (const G<T>& x) const
  {
    G<T> x2 (*this);
    for (E<T> i = x.g2 (); i != x.g1 (); ++i)
      x2.g3 (*i);
    return x2;
  }

  G<T>& operator+= (const G<T>& x)
  {
    for (E<T> i = x.g2 (); i != x.g1 (); ++i)
      g3 (*i);
    return *this;
  }
};

struct H : public G<B>
{
  H () {}
  H (const H& x) : G<B> (x) {}
  H (const G<B>& x) : G<B> (x) {}
};

void foo ();

int
main ()
{
  H a = H () + H ();
  a += H ();
  H b;
  b = H () + H ();
}
