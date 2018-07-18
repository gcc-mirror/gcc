/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

unsigned long f1 (int x)
{
  return x > 0 ? (unsigned long) x : 0;
}

unsigned long f2 (int x, int y)
{
  return x > y ? (unsigned long) x : (unsigned long) y;
}

unsigned long f3 (int x)
{
  return x < 0 ? (unsigned long) x : 0;
}

unsigned long f4 (int x, int y)
{
  return x < y ? (unsigned long) x : (unsigned long) y;
}

unsigned long f5 (unsigned int x, unsigned int y)
{
  return x > y ? (unsigned long) x : (unsigned long) y;
}

unsigned long f6 (unsigned int x, unsigned int y)
{
  return x < y ? (unsigned long) x : (unsigned long) y;
}

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 3 "original"} } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 3 "original"} } */
