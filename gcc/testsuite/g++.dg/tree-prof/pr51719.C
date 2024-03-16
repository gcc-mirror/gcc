// PR tree-optimization/51719
// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-options "-O -fpartial-inlining" }

int
bar (void)
{
  throw 1;
}

int __attribute__ ((noinline, noclone))
foo (int (*f) (void))
{
  try
  {
    return (*f) ();
  }
  catch (...)
  {
  }
  return 0;
}

int
main ()
{
  return foo (bar);
}
