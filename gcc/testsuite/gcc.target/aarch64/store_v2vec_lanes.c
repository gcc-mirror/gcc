/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize" } */

typedef long long v2di __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

void
construct_lane_1 (double *y, v2df *z)
{
  double y0 = y[0] + 1;
  double y1 = y[1] + 2;
  v2df x = {y0, y1};
  z[2] = x;
}

void
construct_lane_2 (long long *y, v2di *z)
{
  long long y0 = y[0] + 1;
  long long y1 = y[1] + 2;
  v2di x = {y0, y1};
  z[2] = x;
}

void
construct_lane_3 (double **py, v2df **pz)
{
  double *y = *py;
  v2df *z = *pz;
  double y0 = y[0] + 1;
  double y1 = y[1] + 2;
  v2df x = {y0, y1};
  z[2] = x;
}

void
construct_lane_4 (long long **py, v2di **pz)
{
  long long *y = *py;
  v2di *z = *pz;
  long long y0 = y[0] + 1;
  long long y1 = y[1] + 2;
  v2di x = {y0, y1};
  z[2] = x;
}

/* We can use the load_pair_lanes<mode> pattern to vec_concat two DI/DF
   values from consecutive memory into a 2-element vector by using
   a Q-reg LDR.  */

/* { dg-final { scan-assembler-times "stp\td\[0-9\]+, d\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "stp\tx\[0-9\]+, x\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-not "ins\t" } } */
