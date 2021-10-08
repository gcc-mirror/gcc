/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-ccp1 -fdump-tree-store-merging" } */

void write64 (void *p)
{
  unsigned *p1 = (unsigned *) __builtin_assume_aligned (p, 8);
  *p1++ = 0;
  unsigned *p2 = (unsigned *) __builtin_assume_aligned (p1, 4);
  *p2++ = 1;
}

/* { dg-final { scan-tree-dump-times "__builtin_assume_aligned" 1 "ccp1" } } */
/* { dg-final { scan-tree-dump "New sequence of 1 stores to replace old one of 2 stores" "store-merging" { target lp64 } } } */
