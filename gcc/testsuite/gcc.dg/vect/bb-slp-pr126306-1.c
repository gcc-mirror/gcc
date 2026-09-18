/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fopt-info-vec" } */

int x[8], y[4];

void foo (void)
{
  /* We want to split the store group after the first two elements, not
     after the first four.  */
  x[2] = 1; /* { dg-optimized "basic block part vectorized using 8 byte vectors" { target vect64 } } */
  x[3] = 2;
  x[4] = y[0]; /* { dg-optimized "basic block part vectorized using 16 byte vectors" { target vect128 } } */
  x[5] = y[1];
  x[6] = y[2];
  x[7] = y[3];
}
