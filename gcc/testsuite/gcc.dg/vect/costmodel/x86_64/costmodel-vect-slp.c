/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-slp-details" } */

#define N 4

int s1[N], s2[N], s3[N];
void escape(int, int, int, int);

void
foo ()
{
  int t1, t2, t3, t4;

  t1 = s1[0] + s2[0] + s3[0];
  t2 = s1[1] + s2[1] + s3[1];
  t3 = s1[2] + s2[2] + s3[2];
  t4 = s1[3] + s2[3] + s3[3];

  s3[0] = t1 - s1[0] * s2[0];
  s3[1] = t2 - s1[1] * s2[1];
  s3[2] = t3 - s1[2] * s2[2];
  s3[3] = t4 - s1[3] * s2[3];

  escape (t1, t2, t3, t4);
}

/* { dg-final { scan-tree-dump-not "vectorization is not profitable" "slp2" } } */
/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
