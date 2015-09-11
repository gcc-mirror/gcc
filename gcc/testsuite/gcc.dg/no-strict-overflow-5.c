/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-optimized" } */

/* Dual of strict-overflow-5.c.  */

/* We can only unroll when using strict overflow semantics.  */

int foo (int i)
{
  int index;
  int r=0;
 
  for (index = i; index <= i+4; index+=2) 
    r++;
 
  return r;
}

/* { dg-final { scan-tree-dump-times "r = 3" 0 "optimized" } } */
