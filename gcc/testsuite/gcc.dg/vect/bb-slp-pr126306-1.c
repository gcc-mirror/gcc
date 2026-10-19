/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int x[8], y[4];

void foo (void)
{
  /* We want to split the store group after the first two elements, not
     after the first four.  */
  x[2] = 1;
  x[3] = 2;
  x[4] = y[0];
  x[5] = y[1];
  x[6] = y[2];
  x[7] = y[3];
}

/* { dg-final { scan-tree-dump "10:8: optimized: basic block part vectorized using 8 byte vectors" "slp2" { target { vect64 || i?86-*-* } } } } */
/* { dg-final { scan-tree-dump "12:8: optimized: basic block part vectorized using 16 byte vectors" "slp2" { target vect128 } } } */
