/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f (char v)
{
  return __builtin_popcount((int)__builtin_bswap16(v));
}

/* We should be able to remove the bswap here as it does not matter
   for the popcount.  */
/* { dg-final { scan-tree-dump-not "bswap" "optimized"} } */
