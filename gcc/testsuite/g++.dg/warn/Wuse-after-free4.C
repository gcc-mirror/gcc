/* PR middle-end/104232 - spurious -Wuse-after-free after conditional free
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char* f (void);

struct A
{
  char *p;
  A (): p () { }
  ~A ()
  {
    __builtin_free (p);           // { dg-bogus "-Wuse-after-free" }
  }
};

int test_no_warn (void)
{
  A px, qx;

  qx.p = f ();
  if (!qx.p)
    return 0;

  px.p = f ();
  return 1;
}
