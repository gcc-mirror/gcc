
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int funsigned (unsigned a)
{
  return 0x1ffffffffL / a == 0;
}

int funsigned2 (unsigned a)
{
  if (a < 1) return 1;
  return (-1 * 0x1ffffffffL) / a == 0;
}

/* { dg-final { scan-tree-dump ": \\\[2, 8589934591\\\]" "evrp" } } */
/* { dg-final { scan-tree-dump ": \\\[-8589934591, -2\\\]" "evrp" } } */

