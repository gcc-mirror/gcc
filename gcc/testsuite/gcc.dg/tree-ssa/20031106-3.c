/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

/* Check for cprop on array elements.  */

void foo (int testarray[])
{
  testarray[0] = 0;
  testarray[1] = 1;
  testarray[0]++;
  testarray[1]++;
  if (testarray[0] != 1)
    link_error ();
  if (testarray[1] != 2)
    link_error ();
}

/* There should be no link_error calls.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
