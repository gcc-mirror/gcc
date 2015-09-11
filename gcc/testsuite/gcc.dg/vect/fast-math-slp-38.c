/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double self[1024];
double a[1024][1024];
double b[1024];

void __attribute__((noinline,noclone))
foo (void)
{
  int i, j;
  for (i = 0; i < 1024; i+=6)
    for (j = 0; j < 1024; j+=6)
      {
	self[i] = self[i] + a[i][j]*b[j];
	self[i+1] = self[i+1] + a[i][j+1]*b[j+1];
      }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" } } */
