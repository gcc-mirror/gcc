/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

char c[4] __attribute__ ((aligned (4)));

struct S {
  char c1;
  char c2;
  char c3;
  char c4;
};

int f1 (void)
{
  return 3 & (__SIZE_TYPE__)&c[1];
}

int f2 (void)
{
  return 3 & (__SIZE_TYPE__)&((struct S *)&c)->c2;
}

/* { dg-final { scan-tree-dump-times "\& 3" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "return 1" 2 "original" } } */
