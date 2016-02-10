// PR c++/69649
// { dg-do compile }
// { dg-options "-O2" }

struct A { virtual void m1 (); };
struct C : A { void m1 () { m1 (); } };
template <class T> struct B
{
  T *t;
  B (T *x) : t (x) { if (t) t->m1 (); }
  B (const B &);
};
struct D : public C {};
struct F : public D
{
  virtual B<D> m2 ();
  virtual B<D> m3 ();
  int m4 ();
};
struct G : F
{
  B<D> m2 ();
  B<D> m3 ();
};
B<D> G::m2 ()
{
  if (m4 () == 0)
    return this;
  return 0;
}
B<D> G::m3 ()
{
  if (m4 () == 0)
    return this;
  return 0;
}
