/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

int x[4];
int j[4];
void foo()
{
  x[0] = (x[0] << j[0]) + j[0];
  x[1] = (x[1] << j[0]) + j[1];
  x[2] = (x[2] << j[0]) + j[2];
  x[3] = (x[3] << j[0]) + j[3];
}

/* The scalar shift argument should be extracted from the available vector.  */
/* { dg-final { scan-tree-dump "BIT_FIELD_REF" "slp2" } } */
/* { dg-final { scan-tree-dump "basic block vectorized" "slp2" } } */
