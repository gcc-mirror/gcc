/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */

typedef char char16[16] __attribute__ ((aligned (16)));
char16 c16[4] __attribute__ ((aligned (4)));

int f5 (int i)
{
  __SIZE_TYPE__ s = (__SIZE_TYPE__)&c16[i];
  /* 0 */
  return 3 & s;
}

/* { dg-final { scan-tree-dump "return 0;" "ccp1" } } */
