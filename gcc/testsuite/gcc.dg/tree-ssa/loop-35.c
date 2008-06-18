/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim-details" } */

int x;
int a[100];

struct a
{
  int X;
  int Y;
};

struct a arr[100];

void test1(int b)
{
  unsigned i;

  /* And here.  */
  for (i = 0; i < 100; i++)
    {
      arr[b+8].X += i;
      arr[b+9].X += i;
    }
}

void test2(struct a *A, int b)
{
  unsigned i;

  /* And here as well.  */
  for (i = 0; i < 100; i++)
    {
      A[b].X += i;
      A[b+1].Y += i;
    }
}

void test3(unsigned long b)
{
  unsigned i;

  /* And here.  */
  for (i = 0; i < 100; i++)
    {
      arr[b+8].X += i;
      arr[b+9].X += i;
    }
}

void test4(struct a *A, unsigned long b)
{
  unsigned i;

  /* And here as well.  */
  for (i = 0; i < 100; i++)
    {
      A[b].X += i;
      A[b+1].Y += i;
    }
}
/* long index not hoisted for avr target PR 36561 */
/* { dg-final { scan-tree-dump-times "Executing store motion of" 8 "lim" { xfail { "avr-*-*" } } } } */
/* { dg-final { scan-tree-dump-times "Executing store motion of" 6 "lim" { target { "avr-*-*" } } } } */
/* { dg-final { cleanup-tree-dump "lim" } } */
