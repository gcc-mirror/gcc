/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

float a[6];

void foo (float x, float y)
{
  a[0] = 1.;
  a[1] = 2.;
  a[2] = 3.;
  a[3] = 4.;
  a[4] = 5.;
  a[5] = x + y;
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
