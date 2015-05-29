/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void foo (int * __restrict__ p, short * __restrict__ q)
{
  p[0] = q[0] + 1;
  p[1] = q[1] + 1;
  p[2] = q[2] + 1;
  p[3] = q[3] + 1;
}

/* { dg-final { scan-tree-dump "basic block vectorized" "slp2" { target vect_hw_misalign } } } */
