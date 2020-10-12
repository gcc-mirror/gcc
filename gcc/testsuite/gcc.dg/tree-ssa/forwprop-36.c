/* { dg-do compile { target int128 } } */
/* { dg-options "-O -fdump-tree-cddce1" } */

typedef unsigned __int128 u128;

u128 a, b;

static inline u128
foo (u128 p1)
{
  p1 += ~b;
  return -p1;
}

int
main ()
{
  u128 x = foo (~0x7fffffffffffffff);
  if (x != 0x8000000000000001)
    __builtin_abort();
  return 0;
}

/* { dg-final { scan-tree-dump "if \\(b.0_\[0-9\]+ != 0\\)" "cddce1" { target { ! mmix-knuth-mmixware } } } } */
/* { dg-final { scan-tree-dump "if \\(b::1_\[0-9\]+ != 0\\)" "cddce1" { target { mmix-knuth-mmixware } } } } */
