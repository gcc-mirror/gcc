/* { dg-do link } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

extern void foo(void);
int a, b;
static int c;
int main()
{
  if (c)
    foo ();
  int *g = &c;
  int **h = &g;
  int ***h1 = &h;
  if (a)
    while (b)
      b = 0;
}

/* { dg-final { scan-tree-dump "Deleted dead store: g = &c;" "dse1" } } */
