/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Adapted from pr39874.c, but reading from signature[0] in both
   functions.  The ranger should be able to set things up for ICF to
   fold the second function entirely.  */

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

/* { dg-final { scan-tree-dump-times " == 15" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "test1 \\(signature.*tail call" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " == 3" "optimized" } } */
