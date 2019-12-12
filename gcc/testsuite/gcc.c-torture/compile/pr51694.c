/* { dg-require-effective-target untyped_assembly } */
/* { dg-require-effective-target indirect_calls } */

void
foo (x, fn)
  void (*fn) ();
{
  int a = baz ((void *) 0, x);
  (*fn) (x, 0);
}

void
bar (void)
{
  void *x = 0;
  foo (x);
}
