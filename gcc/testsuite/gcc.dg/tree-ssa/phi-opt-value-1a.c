/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

[[gnu::const]]
int constcall(int);

int f(int a, int b)
{
  int c = b+a;
  int t = constcall(c);
  int d;
  if (a == 0) d= b; else d= c;
  return constcall(d) + t;
}

/* There should be no if statement and 2 calls to call1. */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */
/* { dg-final { scan-tree-dump-times "constcall " 1 "optimized" } } */

