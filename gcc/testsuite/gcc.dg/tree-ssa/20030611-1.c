/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
 
extern int square (int) __attribute__ ((__const__));
shit(int a)
{
  return square (a) + square (a);

}

/* There should be precisely one call to square.   If there is more than one,
   then the dominator optimizations failed to remove the redundant call.  */
/* { dg-final { scan-tree-dump-times "square" 1 "dom2"} } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
