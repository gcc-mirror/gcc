/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse-details -fexceptions -fnon-call-exceptions -fno-isolate-erroneous-paths-dereference" } */


int foo (int *p, int b)
{
  int *q;
  int i = 1;
  if (b)
    q = &i;
  else
    q = (void *)0;
  *q = 2;
  return i;
}

/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse1"} } */
/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse2"} } */
/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse3"} } */

