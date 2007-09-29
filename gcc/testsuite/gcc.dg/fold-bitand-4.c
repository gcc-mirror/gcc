/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */
/* { dg-options "-fdump-tree-original -fno-common" { target hppa*-*-hpux* } } */

typedef char char4[4] __attribute__ ((aligned (4)));
char4 c4[4] __attribute__ ((aligned (16)));

typedef char char16[16] __attribute__ ((aligned (16)));
char16 c16[4] __attribute__ ((aligned (4)));

int f1 (void)
{
  /* 12 */
  return 15 & (__SIZE_TYPE__)&c4[3];
}

int f2 (int i)
{
  /* Indeterminate */
  return 15 & (__SIZE_TYPE__)&c4[i];
}

int f3 (int i)
{
  /* 0 */
  return 3 & (__SIZE_TYPE__)&c4[i];
}

int f4 (int i)
{
  /* Indeterminate */
  return 7 & (__SIZE_TYPE__)&c16[i];
}

int f5 (int i)
{
  /* 0 */
  return 3 & (__SIZE_TYPE__)&c16[i];
}

/* { dg-final { scan-tree-dump-times "return 12" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\& 15" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 0" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "\& 7" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
