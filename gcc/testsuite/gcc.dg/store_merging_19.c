/* PR tree-optimization/83843 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */
/* { dg-final { scan-tree-dump-times "Merging successful" 3 "store-merging" { target store_merge } } } */

__attribute__((noipa)) void
foo (unsigned char *buf, unsigned char *tab)
{
  tab = __builtin_assume_aligned (tab, 2);
  buf = __builtin_assume_aligned (buf, 2);
  unsigned v = tab[1] ^ (tab[0] << 8);
  buf[0] = ~(v >> 8);
  buf[1] = ~v;
}

__attribute__((noipa)) void
bar (unsigned char *buf, unsigned char *tab)
{
  tab = __builtin_assume_aligned (tab, 2);
  buf = __builtin_assume_aligned (buf, 2);
  unsigned v = tab[1] ^ (tab[0] << 8);
  buf[0] = (v >> 8);
  buf[1] = ~v;
}

__attribute__((noipa)) void
baz (unsigned char *buf, unsigned char *tab)
{
  tab = __builtin_assume_aligned (tab, 2);
  buf = __builtin_assume_aligned (buf, 2);
  unsigned v = tab[1] ^ (tab[0] << 8);
  buf[0] = ~(v >> 8);
  buf[1] = v;
}

int
main ()
{
  volatile unsigned char l1 = 0;
  volatile unsigned char l2 = 1;
  unsigned char buf[2] __attribute__((aligned (2)));
  unsigned char tab[2] __attribute__((aligned (2))) = { l1 + 1, l2 * 2 };
  foo (buf, tab);
  if (buf[0] != (unsigned char) ~1 || buf[1] != (unsigned char) ~2)
    __builtin_abort ();
  buf[0] = l1 + 7;
  buf[1] = l2 * 8;
  bar (buf, tab);
  if (buf[0] != 1 || buf[1] != (unsigned char) ~2)
    __builtin_abort ();
  buf[0] = l1 + 9;
  buf[1] = l2 * 10;
  baz (buf, tab);
  if (buf[0] != (unsigned char) ~1 || buf[1] != 2)
    __builtin_abort ();
  return 0;
}
