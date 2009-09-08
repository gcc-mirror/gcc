/* Testcase by Martin Michlmayr <tbm@cyrius.com> */
/* { dg-do compile } */

extern void *malloc (__SIZE_TYPE__ __size);
typedef struct VMatrix_ VMatrix;
struct VMatrix_
{
  int dim;
  int t2;
};
void uniform_correlation_matrix (VMatrix * v)
{
  double *xbar = ((void *) 0);
  int m = v->dim;
  int i;
  xbar = malloc (m * sizeof *xbar);
  for (i = 0; i < m; i++)
    xbar[i] /= m;
}

/* { dg-final { cleanup-tree-dump "vect" } } */

