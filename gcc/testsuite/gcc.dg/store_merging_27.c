/* PR tree-optimization/88709 */
/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging-details" } */
/* { dg-final { scan-tree-dump "New sequence of \[12] stores to replace old one of 3 stores" "store-merging" } } */

struct S { char buf[8]; };

__attribute__((noipa)) void
bar (struct S *x)
{
  int i;
  for (i = 0; i < 8; i++)
    if (x->buf[i] != ((i == 1) + (i == 3) * 2))
      __builtin_abort ();
}

int
main ()
{
  __attribute__((aligned(8))) struct S s = {};
  s.buf[1] = 1;
  s.buf[3] = 2;
  bar (&s);
  return 0;
}
