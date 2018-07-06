// PR tree-optimization/79267
// { dg-do compile }
// { dg-options "-O3" }

struct A { A (int); };
struct B
{
  virtual void av () = 0;
  void aw ();
  void h () { av (); aw (); }
};
template <class T> struct G : B
{
  T ba;
  G (int, T) : ba (0) {}
  void av () { ba (0); }
};
struct I
{
  B *bc;
  template <class j, class T> I (j, T) try { G<T> (0, 0); } catch (...) {}
  ~I () { bc->h (); }
};
template <class M> struct C { typedef M *i; };
template <class M> struct J
{
  J ();
  template <class O, class T> J (O, T p2) : be (0, p2) {}
  typename C<M>::i operator-> ();
  I be;
};
struct H : A { H () : A (0) {} };
struct D { J<int> d; void q (); };
template <typename = int> class bs;
int z;

void
foo (int p1, int *, int)
{
  if (p1 == 0)
    throw H ();
}

D bar ();
template <typename T> struct L
{
  struct K { K (int); void operator() (int *) { bar ().q (); } };
  static J<T> bp () { bq (0); return J<T>(); }
  template <typename br> static void bq (br) { J<T> (0, K (0)); }
};
struct F
{
  virtual J<int> x (int) { foo (0, 0, 0); J<bs<> > (L<bs<> >::bp ()); return J<int>(); }
};

void
baz ()
{
  if (z)
    {
      J<F> d, e;
      d->x (0);
      e->x (0);
    }
  J<F> v, i, j;
  v->x (0);
  i->x (0);
  j->x (0);
}
