/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre -fno-tree-vrp" } */
/* { dg-additional-options "-fdump-tree-dse1-details" } */

int
f ()
{
  int a;
  int *p = &a;
  *p = 1;
  a = 2;
  return a;
}

/* { dg-final { scan-tree-dump-times "Deleted dead store.*p_1" 1 "dse1"} } */
