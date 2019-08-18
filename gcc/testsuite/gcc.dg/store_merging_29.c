/* PR tree-optimization/88709 */
/* { dg-do run { target int32 } } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging-details" } */
/* { dg-final { scan-tree-dump "New sequence of 3 stores to replace old one of 6 stores" "store-merging" { target { le && { ! arm*-*-* } } } } } */
/* { dg-final { scan-tree-dump "New sequence of \[34] stores to replace old one of 6 stores" "store-merging" { target { be && { ! arm*-*-* } } } } } */

struct T { char a[1024]; };

__attribute__((noipa)) void
bar (struct T *t)
{
  int x = 0x506;
  if (__builtin_memcmp (&t->a[97], &x, sizeof (x)))
    __builtin_abort ();
  __builtin_memset (&t->a[97], '\0', sizeof (x));
  for (int i = 0; i < 8; ++i)
    if (t->a[i] != ((i == 54) + 2 * (i == 52) + 3 * (i == 95) + 4 * (i == 96)))
      __builtin_abort ();
}

int
main ()
{
  struct T t = {};
  t.a[54] = 1;
  t.a[52] = 2;
  t.a[95] = 3;
  t.a[96] = 4;
  int x = 0x506;
  __builtin_memcpy (&t.a[97], &x, sizeof (x));
  bar (&t);
}
