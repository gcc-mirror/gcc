/* { dg-do compile } */
/* { dg-additional-options "-msve-vector-bits=256" { target aarch64_sve } } */

int x[8];

void
f (void)
{
  x[0] /= 2;
  x[1] /= 3;
  x[2] /= 4;
  x[3] /= 5;
  x[4] /= 6;
  x[5] /= 7;
  x[6] /= 8;
  x[7] /= 9;
}

/* We can vectorize the store from a CTOR built from scalar division
   results but ideally we'd like to see vectorizing the load and the
   division as well.  */
/* { dg-final { scan-tree-dump "transform load" "slp2" { xfail *-*-* } } } */
