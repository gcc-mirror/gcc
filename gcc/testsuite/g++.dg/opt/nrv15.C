// PR debug/39086
// { dg-options "-g -O -fno-tree-sra" }

struct A { int v; };

A ax;

struct B
{
  static A f1 () { return ax; }
  static bool f2 ();
  static A f3 ();
};

struct C
{
  A f4 ()
  {
    A x;
    if (__builtin_expect (this->f6 () < this->f12 (), true))
      x = B::f1 ();
    else
      x = this->f7 ();
    return x;
  }
  A f5 ()
  {
    A y;
    if (this->f6 () < this->f12 ())
      y = B::f1 ();
    else
      y = this->f7 ();
    return y;
  }
  void *f6 () const;
  void *f12 () const;
  virtual A f7 ();
};

C *dx;

struct D
{
  C *f8 () const;
};

class E : virtual public D
{
  void f11 ();
  void f9 ();
  void f10 ();
};

struct G
{
  explicit G ();
  operator bool () const;
};

void
E::f11 (void)
{
  A d = B::f3 ();
  d = this->f8 ()->f4 ();
}

void
E::f9 ()
{
  G c;
  if (c)
    {
      const A e = B::f3 ();
      C * f = this->f8 ();
      A d = f->f5 ();
      if (B::f2 ())
	;
      else if (B::f2 ())
	f->f4 ();
    }
}

void
E::f10 ()
{
  G c;
  if (c)
    {
      const A e = B::f3 ();
      C * f = this->f8 ();
      A d = f->f5 ();
      if (B::f2 ())
	;
      else if (B::f2 ())
	f->f4 ();
    }
}
