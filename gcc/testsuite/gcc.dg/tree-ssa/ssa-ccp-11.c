/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Test for CPROP across a DAG. */

int test111 (int param)
{
  int a, b, c;
  if (param) {
    a = 3;
    b = 2;
  }
  else {
    a = 2;
    b = 3;
  }
  c = a + b;
  if (c != 5)
    return 2;
  return 0;
}

int test1111 (int param)
{
  _Bool a, b, c;
  if (param) {
    a = 1;
    b = 0;
  }
  else {
    a = 0;
    b = 1;
  }
  c = a && b;
  if (c)
    return 2;
  return 0;
}

/* All ifs should be eliminated. */
/* { dg-final { scan-tree-dump-times "if " 0 "optimized" } } */
