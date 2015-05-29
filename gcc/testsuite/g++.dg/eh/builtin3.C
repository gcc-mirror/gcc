// Without explicit prototype, we need to assume the builtin can
// throw for builtins that at least on one platform can throw.
// { dg-do compile }
// { dg-options "-fdump-tree-eh" }

extern void callme (void) throw();

int
bar (int i)
{
  try {
    __builtin_printf ("foo %d\n", i);
  } catch (...) {
    callme();
  }
}

/* { dg-final { scan-tree-dump-times "resx" 1 "eh" } } */
