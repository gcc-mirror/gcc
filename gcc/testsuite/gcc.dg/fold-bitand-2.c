/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

struct {
  char c1;
  char c2;
  char c3;
  char c4;
} s __attribute__ ((aligned (4)));

unsigned f1 (void)
{
  return 3 & (__SIZE_TYPE__)&s.c1;
}

unsigned f2 (void)
{
  return 3 & (__SIZE_TYPE__)&s.c2;
}

unsigned f3 (void)
{
  return 3 & (__SIZE_TYPE__)&s.c3;
}

unsigned f4 (void)
{
  return 3 & (__SIZE_TYPE__)&s.c4;
}

unsigned f5 (void)
{
  return 4 & (__SIZE_TYPE__)&s.c1;
}

/* { dg-final { scan-tree-dump-times "\& 3" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "\& 4" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 1" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 3" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
