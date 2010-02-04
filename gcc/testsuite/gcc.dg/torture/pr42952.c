/* { dg-do run } */
/* { dg-options "-fno-tree-ccp -fno-tree-fre" } */

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
