/* PR tree-optimization/111280 */
/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target clzsi } */
/* { dg-require-effective-target ctzsi } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* The first value that the loops test is T shifted by one.  It can be
   zero, so its count needs a value at zero.  */

__attribute__((noipa)) int
clz_loop (unsigned int t)
{
  int b = 0;
  do
    b++;
  while ((t >>= 1) != 0);
  return b;
}

__attribute__((noipa)) int
ctz_loop (unsigned int t)
{
  int b = 0;
  do
    b++;
  while ((t <<= 1) != 0);
  return b;
}

/* The loop is entered only with a nonzero T, so the count needs no value
   at zero.  */

__attribute__((noipa)) int
clz_guarded (unsigned int t)
{
  int b = 0;
  while (t)
    {
      t >>= 1;
      b++;
    }
  return b;
}

int
main ()
{
  if (clz_loop (0) != 1 || clz_loop (1) != 1 || clz_loop (6) != 3
      || clz_loop (0x80000000u) != 32)
    __builtin_abort ();
  if (ctz_loop (0) != 1 || ctz_loop (0x80000000u) != 1 || ctz_loop (6) != 31
      || ctz_loop (1) != 32)
    __builtin_abort ();
  if (clz_guarded (0) != 0 || clz_guarded (6) != 3
      || clz_guarded (0x80000000u) != 32)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times {\.CLZ \(_\d+, 32\)} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.CTZ \(_\d+, 32\)} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.CLZ \(t_\d+\(D\)\)} 1 "optimized" } } */
