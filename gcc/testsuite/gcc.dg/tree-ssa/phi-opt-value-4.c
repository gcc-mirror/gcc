/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-fre3 -fdump-tree-phiopt1 -fdump-tree-optimized" } */

[[gnu::const]]
int constcall(int);

int fdiv(int a, int b)
{
  int c = b/a;
  int t = constcall(c);
  int d;
  if (a == 1) d = b; else d = c;
  return constcall(d) + t;
}
int fmult(int a, int b)
{
  int c = b*a;
  int t = constcall(c);
  int d;
  if (a == 1) d = b; else d = c;
  return constcall(d) + t;
}
int fand(int a, int b)
{
  int c = b&a;
  int t = constcall(c);
  int d;
  if (a == -1) d = b; else d = c;
  return constcall(d) + t;
}

/* Should be able to optimize away the if statements in phiopt1. */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" } } */
/* fre3 should be optimize each function to just `return constcall(a OP b) * 2;`. */
/* { dg-final { scan-tree-dump-times "constcall " 3 "fre3" } } */
/* { dg-final { scan-tree-dump-times "constcall " 3 "optimized" } } */
