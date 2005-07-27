/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */
int f1(int a,int b,int c,int d)
{
  return ((a&d)|(b&c)) ^ (b&c);
}

int f2(int a,int b,int c,int d)
{
  return (b&c) ^ ((a&d)|(b&c));
}

int f3(int a,int b,int c,int d)
{
  return ((b&c)|(a&d)) ^ (b&c);
}

int f4(int a,int b,int c,int d)
{
  return (b&c) ^ ((b&c)|(a&d));
}

/* There should be no ^, 4 ~ and 12 &. */
/* { dg-final { scan-tree-dump-times "\\^" 0 "gimple"} } */
/* { dg-final { scan-tree-dump-times "~" 4 "gimple"} } */
/* { dg-final { scan-tree-dump-times "&" 12 "gimple"} } */
