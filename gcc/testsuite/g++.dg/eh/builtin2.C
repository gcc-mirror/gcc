// Verify that if explicit prototype for builtin is present with throw(),
// neither the normal builtin nor __builtin_* variant can throw exceptions.
// { dg-do compile }
// { dg-options "-fdump-tree-eh" }

extern "C" int printf (const char *, ...) throw();

extern void callme (void) throw();

int
foo (int i)
{
  try {
    printf ("foo %d\n", i);
  } catch (...) {
    callme();
  }
}

int
bar (int i)
{
  try {
    __builtin_printf ("foo %d\n", i);
  } catch (...) {
    callme();
  }
}

/* { dg-final { scan-tree-dump-times "resx" 0 "eh" } } */
