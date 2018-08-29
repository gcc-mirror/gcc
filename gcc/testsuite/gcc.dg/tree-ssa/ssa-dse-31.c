/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

void g();
void f(int n, char *p)
{
  *p = 0;
  if (n)
    {
      *p = 1;
      g();
    }
  *p = 2;
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 1 "dse1" } } */
