/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

float a[32000], b[32000], c[32000], d[32000];
float aa[256][256], bb[256][256], cc[256][256];

void
s2275 ()
{
  for (int i = 0; i < 256; i++)
    {
      for (int j = 0; j < 256; j++)
	{
	  aa[j][i] = aa[j][i] + bb[j][i] * cc[j][i];
	}
      a[i] = b[i] + c[i] * d[i];
    }
}

/* { dg-final { scan-assembler-times {e32,m8} 1 } } */
/* { dg-final { scan-assembler-not {e32,m4} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-not "Preferring smaller LMUL loop because it has unexpected spills" "vect" } } */
