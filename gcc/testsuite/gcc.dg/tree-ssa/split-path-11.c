/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-paths -fdump-tree-split-paths-details -w" } */

void foo(unsigned long *M)
{
  for (unsigned long k = 0; k < 227; ++k)
    {
      unsigned long y =
	((M[k] & 0xffffffff80000000) | (M[k + 1] & 0x7fffffff));
      M[k] = (M[k + 397] ^ (y >> 1) ^ ((y & 1) ? 2567483615 : 0));
    }
}

/* { dg-final { scan-tree-dump-times "join point for if-convertable half-diamond" 1 "split-paths" } } */
