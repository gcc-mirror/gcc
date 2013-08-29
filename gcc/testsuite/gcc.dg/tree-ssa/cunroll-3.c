/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details" } */
int a[1];
test(int c)
{ 
  int i;
  for (i=0;i<c;i++)
    {
      a[i]=5;
    }
}
/* If we start duplicating headers prior curoll, this loop will have 0 iterations.  */

/* { dg-final { scan-tree-dump "loop with 2 iterations completely unrolled" "cunrolli"} } */
/* { dg-final { cleanup-tree-dump "cunrolli" } } */
