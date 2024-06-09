/* { dg-do compile } */
/* { dg-additional-options "-std=gnu99 -O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -fdump-tree-slp-details" } */

void
__attribute__ ((noipa))
f (int *restrict x, short *restrict y, int *restrict res)
{
  res[0] = x[0] == 1 & y[0] == 2;
  res[1] = x[1] == 1 & y[1] == 2;
  res[2] = x[2] == 1 & y[2] == 2;
  res[3] = x[3] == 1 & y[3] == 2;
  res[4] = x[4] == 1 & y[4] == 2;
  res[5] = x[5] == 1 & y[5] == 2;
  res[6] = x[6] == 1 & y[6] == 2;
  res[7] = x[7] == 1 & y[7] == 2;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "slp2" } } */
