/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a[1024];
void foo (int x)
{
  for (int i = 0; i < 1024; i += 2)
    {
      a[i] = x;
      a[i+1] = 1;
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
