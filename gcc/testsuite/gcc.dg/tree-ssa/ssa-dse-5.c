/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int x; 
 
int 
f1 (int i, int j, int k) 
{ 
  int *p = k ? &i : &j; 
  i = 3;
  *p = 5; 
  x = j; 
} 

/* The assignment "i = 3" is partially dead.  Our DSE pass doesn't handle
   detection of partially dead assignments.

   There's two outputs which would indicate that the optimization was
   performed.

   If we used block copying to detect and eliminate the partially dead
   store, then we should see an assignment "i = 5" somewhere in the
   dump file.

   Another approach would be to redirect the path from the true arm
   of the first conditional so that it reaches the statement *p = 5
   rather than i = 3.  */

/* { dg-final { scan-tree-dump-times "i = 5" 1 "optimized" { xfail *-*-* }} } */
/* { dg-final { scan-tree-dump-times "<L.*>:;\[\n\t \]*\\*p = 5" 1 "optimized" { xfail *-*-*}} } */

/* { dg-final { cleanup-tree-dump "optimized" } } */

