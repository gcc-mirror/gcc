/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-whole-program"  } */
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
/* We optimize out this call just after early passes.  Unfortunately
   this unreachable removal is not logged in dump file.  */
/* { dg-final { scan-ipa-dump-not "OBJ_TYPE_REF" "whole-program" } } */
/* { dg-final { cleanup-ipa-dump "whole-program" } } */
