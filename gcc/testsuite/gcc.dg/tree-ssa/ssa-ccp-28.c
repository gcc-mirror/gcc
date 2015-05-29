/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ccp1" } */

extern void abort (void);

static int g[1];

static int * const p = &g[0];
static int * const q = &g[0];

int main(void)
{
  g[0] = 1;
  *p = 0;
  *p = *q;
  if (g[0] != 0)
    abort ();
  return 0;
}

/* We should have replaced all loads from p and q with the constant
   initial value.  */

/* { dg-final { scan-tree-dump-times "= p;" 0 "ccp1" } } */
/* { dg-final { scan-tree-dump-times "= q;" 0 "ccp1" } } */
