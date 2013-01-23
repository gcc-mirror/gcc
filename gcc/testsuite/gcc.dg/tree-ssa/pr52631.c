/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1-details" } */

unsigned f(unsigned a)
{
  unsigned b = a >> 31;
  return b&1;
}

/* We want to verify that we replace the b & 1 with b.  */
/* { dg-final { scan-tree-dump-times "Replaced b_\[0-9\]+ & 1 with b_\[0-9\]+ in" 1 "fre1"} } */
 
/* { dg-final { cleanup-tree-dump "fre1" } } */

