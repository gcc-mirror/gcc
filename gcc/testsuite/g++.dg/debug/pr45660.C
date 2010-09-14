// PR debug/45660
// { dg-do compile }
// { dg-options "-g -fno-inline" }

void
test ()
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
