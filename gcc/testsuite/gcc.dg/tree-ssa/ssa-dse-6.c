/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */

int foo11 (int c)
{
  static int local1, local2;
  local1 = 0;
  local2 += c;
  local1 = 2;
  local2++;
  return local1 + local2;
}

/* There should only be one assignment to local1 and local2.  */
/* { dg-final { scan-tree-dump-times "local1 = " 1 "dse1"} } */
/* { dg-final { scan-tree-dump-times "local2 = " 1 "dse1"} } */

/* { dg-final { cleanup-tree-dump "dse1" } } */
