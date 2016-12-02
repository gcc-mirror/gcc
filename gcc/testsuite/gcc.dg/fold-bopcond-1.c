/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt" } */

int foo1 (unsigned short a[], unsigned int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (unsigned short)(x <= 32768 ? x + 32768 : 0);
    }
  return x;
}

int foo2 (unsigned short a[], unsigned int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (unsigned short)(x < 32768 ? x + 32768 : 0);
    }
  return x;
}

int foo3 (unsigned short a[], unsigned int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (unsigned short)(x < 1000 ? x - 1000 : 0);
    }
  return x;
}

int foo4 (unsigned short a[], unsigned int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (unsigned short)(x <= 2 ? x + 999 : 1001);
    }
  return x;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR " 4 "ifcvt" } } */
