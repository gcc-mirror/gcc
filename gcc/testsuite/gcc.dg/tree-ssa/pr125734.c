/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
nlz32 (int b)
{
  int c = sizeof(b) * 8;
  if (b < 0)
    return -1;
  while (b != 0)
    {
      b >>= 1;
      c--;
    }
  return c;
}

int
nlz32_negative (int b)
{
  int c = sizeof(b) * 8;

  while (b != 0)
    {
      b >>= 1;
      c--;
    }

  return c;
}

/* { dg-final { scan-tree-dump-times "\\.CLZ" 1 "optimized" } } */
