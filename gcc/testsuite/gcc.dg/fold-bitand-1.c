/* { dg-do compile } */
/* { dg-options "-w -fdump-tree-original" } */

char c1 __attribute__ ((aligned (1)));
char c2 __attribute__ ((aligned (2)));
char c4 __attribute__ ((aligned (4)));
char c8 __attribute__ ((aligned (8))); 
unsigned f1(void)
{
  return 3 & (__SIZE_TYPE__)&c1;
}

unsigned f2(void)
{
  return 3 & (__SIZE_TYPE__)&c2;
}

unsigned f3(void)
{
  return 3 & (__SIZE_TYPE__)&c4;
}

unsigned f4(void)
{
  return 3 & (__SIZE_TYPE__)&c8;
}

/* { dg-final { scan-tree-dump-times "\&c1 \& 3" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\&c2 \& 3" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\&c4 \& 3" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "\&c8 \& 3" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "return 0" 2 "original" } } */
