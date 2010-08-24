/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize -fdump-tree-ifcvt-stats" { target *-*-* } } */

void
dct_unquantize_h263_inter_c (short *block, int n, int qscale, int nCoeffs)
{
  int i, level, qmul, qadd;

  qadd = (qscale - 1) | 1;
  qmul = qscale << 1;

  for (i = 0; i <= nCoeffs; i++)
    {
      level = block[i];
      if (level < 0)
	level = level * qmul - qadd;
      else
	level = level * qmul + qadd;
      block[i] = level;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
/* { dg-final { cleanup-tree-dump "ifcvt" } } */
