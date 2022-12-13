/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

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

/* { dg-final { scan-tree-dump-times "Global Exported" 2 "vrp1" } } */
