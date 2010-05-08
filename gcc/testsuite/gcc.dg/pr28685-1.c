/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" }  */

/* Should produce <=.  */
int test1 (int a, int b)
{
  return (a < b || a == b);
}

/* Should produce <=.  */
int test2 (int a, int b)
{
  int lt = a < b;
  int eq = a == b;

  return (lt || eq);
}

/* Should produce <= (just deleting redundant test).  */
int test3 (int a, int b)
{
  int lt = a <= b;
  int eq = a == b;

  return (lt || eq);
}

/* Should produce <= (operands reversed to test the swap logic).  */
int test4 (int a, int b)
{
  int lt = a < b;
  int eq = b == a;

  return (lt || eq);
}

/* Should produce constant 0.  */
int test5 (int a, int b)
{
  int lt = a < b;
  int eq = a == b;

  return (lt && eq);
}

/* { dg-final { scan-tree-dump-times " <= " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " < " "optimized" } } */
/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
