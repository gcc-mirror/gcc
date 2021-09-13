/* PR/101293 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

struct X { int i; int j; };

void foo(struct X *x, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int *p = &x->j;
      int tem = *p;
      x->j += tem * i;
    }
}

/* Make sure LIM can handle unifying MEM[x, 4] and MEM[x].j  */
/* { dg-final { scan-tree-dump "Executing store motion" "lim2" } } */
