/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-inline"  } */
double foo ();
struct B
{
  bool b1 () { return b3 (); }
  void b2 ();
  virtual bool b3 ();
};
struct C
{
  C () {}
  bool
  c1 (float x, float y)
  {
    if (x != c3 || y != c4)
      c2.b2 ();
    return c2.b1 ();
  }
  B c2;
  float c3, c4;
};

void
bar ()
{
  static C c;
  c.c1 (60, (int) foo ());
}
/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
