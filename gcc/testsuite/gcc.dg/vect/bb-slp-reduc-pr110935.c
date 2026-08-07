/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

double vals[16];
double test ()
{
  vals[0]++;
  return vals[2] + vals[4] + vals[1] + vals[3];
}

/* { dg-final { scan-tree-dump "optimized: basic block part vectorized" "slp2" { target { vect_double && vect_hw_misalign } } } } */
