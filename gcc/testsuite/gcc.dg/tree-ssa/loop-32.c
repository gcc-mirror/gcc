/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim-details" } */

int x;
int a[100];

struct a
{
  int X;
  int Y;
};

void bla(void);

void test1(void)
{
  unsigned i;

  /* We should perform store motion here.  */
  for (x = 0; x < 100; x++)
    a[x] = x;
}

void test2(void)
{
  unsigned i;

  /* But not here.  */
  for (x = 0; x < 100; x++)
    bla ();
}

void test3(struct a *A)
{
  unsigned i;

  /* But we should here (using base + offset analysis).  */
  for (i = 0; i < 100; i++)
    {
      A[5].X += i;
      A[5].Y += i;
    }
}

/* { dg-final { scan-tree-dump-times "Executing store motion of" 3 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim\[1-2\]" } } */
