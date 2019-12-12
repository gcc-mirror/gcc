/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-ldist-optimized" } */

void foo(char* restrict dst, const char* buf)
{
  for (int i=0; i<8; ++i)
    *dst++ = *buf++;
}

/* { dg-final { scan-tree-dump "split to 0 loops and 1 library calls" "ldist" } } */
