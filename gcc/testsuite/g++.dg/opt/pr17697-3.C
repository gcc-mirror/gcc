// PR tree-optimization/17697
// { dg-do run }
// { dg-options "-O2" }

extern "C" extern int strcmp (const char *s, const char *t);

namespace A
{
  extern int strcmp (const char *s, const char *t);
}

inline int
A::strcmp (const char *s, const char *t)
{
  return ::strcmp (s, t);
}

int
foo (char *x)
{
  return A::strcmp ("", x);
}

int
main ()
{
  return foo ("") != 0 || foo ("a") == 0;
}
