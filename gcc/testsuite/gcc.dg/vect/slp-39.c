/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double x[1024], y[1024], z[1024];
void foo (double w)
{
  int i;
  for (i = 0; i < 1023; i+=2)
    {
      z[i] = x[i] + 1;
      z[i+1] = x[i+1] + w;
    }
}
void bar (double w)
{
  int i;
  for (i = 0; i < 1023; i+=2)
    {
      z[i] = x[i] + w;
      z[i+1] = x[i+1] + 1;
    }
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
