/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-mavx" { target avx } } */

double x[10], y[6], z[4];

void foo ()
{
  x[0] = y[0];
  x[1] = y[1];
  x[2] = y[2];
  x[3] = y[3];
  x[4] = y[4];
  x[5] = y[5];
  x[6] = z[0] + 1.;
  x[7] = z[1] + 1.;
  x[8] = z[2] + 1.;
  x[9] = z[3] + 1.;
}

/* We want to have the store group split into 4, 2, 4 when using 32byte vectors.
   Unfortunately it does not work when 64-byte vectors are available.  */
/* { dg-final { scan-tree-dump-not "from scalars" "slp2" { xfail amdgcn-*-* } } } */
