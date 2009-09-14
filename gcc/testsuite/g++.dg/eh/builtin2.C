// Verify that if explicit prototype for builtin is present with throw(),
// neither the normal builtin nor __builtin_* variant can throw exceptions.
// { dg-do compile }
// { dg-options "-fdump-tree-eh" }

extern "C" int printf (const char *, ...) throw();

struct A { A (); ~A (); int i; };

int
foo ()
{
  A a;
  printf ("foo %d\n", a.i);
}

int
bar ()
{
  A a;
  __builtin_printf ("foo %d\n", a.i);
}

/* { dg-final { scan-tree-dump-times "resx" 0 "eh" } } */
/* { dg-final { cleanup-tree-dump "eh" } } */
