/* { dg-do compile } */
/* { dg-additional-options "-ftree-slp-vectorize" } */

int ai[2][8];
void bar (int *);
void
br (void)
{
  int qp[9];
  bar (qp);
  ai[0][0] = qp[0] + qp[1] + 1 >> 1;
  ai[0][1] = qp[1] + qp[2] + 1 >> 1;
  ai[0][2] = qp[2] + qp[3] + 1 >> 1;
  ai[0][3] = qp[3] + qp[4] + 1 >> 1;
  ai[0][4] = qp[4] + qp[5] + 1 >> 1;
  ai[0][5] = qp[5] + qp[6] + 1 >> 1;
  ai[0][6] = qp[6] + qp[7] + 1 >> 1;
  ai[0][7] = qp[7] + qp[8] + 1 >> 1;
  ai[1][0] = qp[0] + qp[1] + 2 * qp[0] + 1 >> 2;
  ai[1][1] = qp[0] + qp[2] + 2 * qp[1] + 1 >> 2;
  ai[1][2] = qp[1] + qp[3] + 2 * qp[2] + 1 >> 2;
  ai[1][3] = qp[2] + qp[4] + 2 * qp[3] + 1 >> 2;
  ai[1][4] = qp[3] + qp[5] + 2 * qp[4] + 1 >> 2;
  ai[1][5] = qp[4] + qp[6] + 2 * qp[5] + 1 >> 2;
  ai[1][6] = qp[5] + qp[7] + 2 * qp[6] + 1 >> 2;
  ai[1][7] = qp[6] + qp[8] + 2 * qp[7] + 1 >> 2;
}
