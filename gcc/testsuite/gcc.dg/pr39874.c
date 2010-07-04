/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" }  */

extern void func();

void test1(char *signature)
{
  char ch = signature[0];
  if (ch == 15 || ch == 3)
  {
    if (ch == 15) func();
  }
}


void test2(char *signature)
{
  char ch = signature[0];
  if (ch == 15 || ch == 3)
  {
    if (ch > 14) func();
  }
}

/* { dg-final { scan-tree-dump-times " == 15" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not " == 3" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */


