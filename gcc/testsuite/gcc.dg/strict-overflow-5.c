/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -fdump-tree-optimized" } */

/* We can only unroll when using strict overflow semantics.  */

int foo (int i)
{
  int index;
  int r=0;
 
  for (index = i; index <= i+4; index+=2) 
    r++;
 
  return r;
}

/* { dg-final { scan-tree-dump "return 3" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

