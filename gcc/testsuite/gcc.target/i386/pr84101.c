/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-slp2-details" } */

typedef struct uint64_pair uint64_pair_t ;
struct uint64_pair
{
  unsigned long w0 ;
  unsigned long w1 ;
} ;

uint64_pair_t pair(int num)
{
  uint64_pair_t p ;

  p.w0 = num << 1 ;
  p.w1 = num >> 1 ;

  return p ;
}

/* See PR92266 for the XFAIL.  */
/* { dg-final { scan-tree-dump-not "basic block vectorized" "slp2" { xfail ilp32 } } } */
