// Copy of g++.dg/debug/pr45660
// { dg-lto-do link }
// { dg-lto-options { { -g -flto } } }

int
main ()
{
  struct S
  {
    typedef void (**T) (void);
    static T i (void) { return 0; }
  };
  S s;
  if (s.i ())
    *s.i () = 0;
}
