/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-require-effective-target vect_double } */

double a[1024], b[1024], c[1024];

void foo()
{
  for (int i = 0; i < 256; ++i)
    {
      a[2*i] = a[2*i] + b[2*i] - c[2*i];
      a[2*i+1] = a[2*i+1] - b[2*i+1] - c[2*i+1];
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
