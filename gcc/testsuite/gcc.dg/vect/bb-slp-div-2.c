/* { dg-do compile } */

int x[4], y[4], z[4];

void
f (void)
{
  x[0] += y[0] / z[0] * 2;
  x[1] += y[1] / z[1] * 2;
  x[2] += y[2] / z[2] * 2;
  x[3] += y[3] / z[3] * 2;
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" { target vect_int } } } */
