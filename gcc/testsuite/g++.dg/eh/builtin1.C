// Verify that if explicit prototype for builtin is present without throw(),
// both the normal builtin and __builtin_* variant are expected to be
// able to throw exceptions.
// { dg-do compile }
// { dg-options "-fdump-tree-eh" }

extern "C" int printf (const char *, ...);

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

/* { dg-final { scan-tree-dump-times "resx" 2 "eh" } } */
