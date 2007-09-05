// Verify that if explicit prototype for builtin is present without throw(),
// both the normal builtin and __builtin_* variant are expected to be
// able to throw exceptions.
// { dg-do compile }
// { dg-options "-fdump-tree-eh" }

extern "C" int printf (const char *, ...);

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

/* { dg-final { scan-tree-dump-times "resx 1" 2 "eh" } } */
/* { dg-final { cleanup-tree-dump "eh" } } */
