/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

int f1 (int x)
{
  return x & 1 ? (x - 1) : (x | 1);
}

int f2 (int x)
{
  return x & 2 ? (x - 2) : (x | 2);
}

int f3 (int x)
{
  return x & 3 ? (x - 3) : (x | 3);
}

/* { dg-final { scan-tree-dump-times "x \\^ 1" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "x \\^ 2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "x \\^ 3" 0 "original" } } */
