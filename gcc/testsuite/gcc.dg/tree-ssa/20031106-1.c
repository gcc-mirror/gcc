/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

/* Check for dead stores to an array.  */

void foo (int testarray[])
{
  testarray[0] = 0;
  testarray[0]++;
  if (testarray[0] != 1)
    link_error ();
}

/* There should be only one reference to "testarray".  */
/* { dg-final { scan-tree-dump-times "testarray" 1 "optimized" { xfail *-*-* } } } */

/* There should be no link_error calls.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
