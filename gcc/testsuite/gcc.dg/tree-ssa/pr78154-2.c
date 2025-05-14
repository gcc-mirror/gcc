/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-slim -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" { keeps_null_pointer_checks } } */

void foo (void *, __SIZE_TYPE__) __attribute__((nonnull_if_nonzero (1, 2)));
void baz (void);

void
bar (void *a, void *b, void *c, void *d, void *e, __SIZE_TYPE__ n)
{
  foo (a, 42);
  if (a == 0)
    __builtin_abort ();
  if (n)
    {
      foo (b, n);
      if (b == 0)
	__builtin_abort ();
    }
  if (n >= 42)
    {
      foo (c, n - 10);
      if (c == 0)
	__builtin_abort ();
    }
  foo (d, 0);
  if (d == 0)
    baz ();
  if (n != 42)
    {
      foo (e, n);
      if (e == 0)
	baz ();
    }
}

/* { dg-final { scan-tree-dump-not "__builtin_abort" "evrp" } } */
/* { dg-final { scan-tree-dump-times "baz \\\(" 2 "evrp" } } */
