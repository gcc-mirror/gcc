/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -fdump-tree-final_cleanup" } */

/* We can only unroll when using strict overflow semantics.  */

int foo (int i)
{
  int index;
  int r=0;
 
  for (index = i; index <= i+4; index+=2) 
    r++;
 
  return r;
}

/* { dg-final { scan-tree-dump-times "r = 3" 1 "final_cleanup" } } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */

