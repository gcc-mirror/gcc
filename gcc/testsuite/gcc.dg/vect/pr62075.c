/* { dg-do compile } */

int a[16][2];
struct A
{
  int b[16][2];
  int c[16][1];
};

void
foo (struct A *x)
{
  int i;
  for (i = 0; i < 16; ++i)
    {
      x->b[i][0] = a[i][0];
      x->c[i][0] = 0 != a[i][0];
      x->b[i][1] = a[i][1];
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
