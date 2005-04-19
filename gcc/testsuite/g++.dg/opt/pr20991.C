// PR middle-end/20991
// { dg-options "-O2" }
// { dg-do compile }

struct S
{
  virtual inline int foo () const;
  virtual inline bool bar () const;
  virtual int baz (int) const;
};

inline int S::foo () const
{
  return 1;
}

inline bool S::bar () const
{
  return foo () == 0;
}

void A ()
{
  S s;
  if (s.bar ())
    s.foo ();
}

void B ()
{
  S s;
  if (s.bar ())
    s.foo ();
}
