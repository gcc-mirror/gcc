// PR tree-optimization/17724
// { dg-do compile }
// { dg-options "-O2" }

namespace N { char *strcpy (char *, const char *); }
extern "C" char *strcpy (char *, const char *);
inline char *N::strcpy (char *s, const char *t) { return ::strcpy (s, t); }

struct S { ~S (); };
int foo ();

int
main ()
{
  S s;
  int a;
  char b[64];
  N::strcpy (b, "ABCDEFGHIJKLM");
  while ((a = foo ()) != -1)
    if (a)
      return -1;
  return 0;
}
