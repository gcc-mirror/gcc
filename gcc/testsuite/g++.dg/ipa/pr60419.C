// PR middle-end/60419
// { dg-do compile }
// { dg-options "-O2" }

struct C
{
};

struct I : C
{
  I ();
};

struct J
{
  void foo ();
  J ();
  virtual void foo (int &, int);
};

template <class>
struct D
{
  virtual void foo (I &) const;
  void bar ()
  {
    I p;
    foo (p);
  }
};

struct K : J, public D<int>
{
};

struct F
{
  K *operator->();
};

struct N : public K
{
  void foo (int &, int);
  I n;
  void foo (I &) const {}
};

struct L : J
{
  F l;
};

struct M : F
{
  L *operator->();
};

struct G
{
  G ();
};

M h;

G::G ()
try
{
  N f;
  f.bar ();
  throw;
}
catch (int)
{
}

void
baz ()
{
  h->l->bar ();
}
