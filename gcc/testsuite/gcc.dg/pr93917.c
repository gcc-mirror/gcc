/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdump-tree-vrp2 -fdump-tree-optimized-alias" } */

void f3(int n);

void f1(int n)
{
  if(n<0)
    __builtin_unreachable();
  f3(n);
}

void f2(int*n)
{
  if(*n<0)
    __builtin_unreachable();
  f3 (*n);
}

/* { dg-final { scan-tree-dump-times "Global Export.*0, \\+INF" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable" 0 "vrp2" } } */
/* { dg-final { scan-tree-dump-times "0, \\+INF" 2 "optimized" } } */
