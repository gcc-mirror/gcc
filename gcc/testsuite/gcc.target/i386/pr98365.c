/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" } } */
short foo1 (short* a, short* c, int n)
{
  int i;
  short cnt=0;
  for (int i = 0;i != n; i++)
    if (a[i] == c[i])
      cnt++;
  return cnt;
}

char foo2 (char* a, char* c, int n)
{
  int i;
  char cnt=0;
  for (int i = 0;i != n; i++)
    if (a[i] == c[i])
      cnt++;
  return cnt;
}
