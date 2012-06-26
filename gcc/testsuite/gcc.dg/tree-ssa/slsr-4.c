/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-slsr -fdump-tree-optimized" } */

void foo (int);

int
f (int i)
{
  int x, y;

  x = i * 4;
  y = x * 10;
  foo (y);

  i = i + 5;
  x = i * 4;
  y = x * 10;
  foo (y);

  i = i - 4;
  x = i * 4;
  y = x * 10;
  foo (y);
}

/* { dg-final { scan-tree-dump-times "\\* 4" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "\\* 10" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "\\+ 20;" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "\\+ 200" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "\\- 16;" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "\\- 160" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "\\* 4" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\* 10" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 200" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 40" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "slsr" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
