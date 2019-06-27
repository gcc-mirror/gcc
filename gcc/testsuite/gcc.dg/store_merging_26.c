/* PR tree-optimization/90271 */
/* { dg-do run { target int32 } } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging-details" } */
/* { dg-final { scan-tree-dump "New sequence of 1 stores to replace old one of 2 stores" "store-merging" } } */

__attribute__((noipa)) void
foo (int *x)
{
  asm volatile ("" : : "r" (x) : "memory");
}

__attribute__((noipa)) int
bar ()
{
  int x;
  foo (&x);
  x = 3;
  ((char *) &x)[1] = 1;
  foo (&x);
  return x;
}

int
main ()
{
  int x;
  foo (&x);
  x = 3;
  foo (&x);
  ((char *) &x)[1] = 1;
  foo (&x);
  if (x != bar ())
    __builtin_abort ();
  return 0;
}
