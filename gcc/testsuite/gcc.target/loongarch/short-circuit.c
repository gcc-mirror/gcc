/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-gimple" } */

int
short_circuit (float *a)
{
  float t1x = a[0];
  float t2x = a[1];
  float t1y = a[2];
  float t2y = a[3];
  float t1z = a[4];
  float t2z = a[5];

  if (t1x > t2y  || t2x < t1y  || t1x > t2z || t2x < t1z || t1y > t2z || t2y < t1z)
    return 0;

  return 1;
}
/* { dg-final { scan-tree-dump-times "if" 6 "gimple" } } */
