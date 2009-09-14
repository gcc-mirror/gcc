// Without explicit prototype, we need to assume the builtin can
// throw for builtins that at least on one platform can throw.
// { dg-do compile }
// { dg-options "-fdump-tree-eh" }

struct A { A (); ~A (); int i; };

int
bar ()
{
  A a;
  __builtin_printf ("foo %d\n", a.i);
}

/* { dg-final { scan-tree-dump-times "resx" 1 "eh" } } */
/* { dg-final { cleanup-tree-dump "eh" } } */
