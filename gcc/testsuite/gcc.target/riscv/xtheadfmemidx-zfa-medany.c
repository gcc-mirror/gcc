/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" "-O0" "-O1" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv32gc_zfa_xtheadfmemidx_xtheadmemidx -mabi=ilp32d -mcmodel=medany" } */

typedef union {
  double v;
  unsigned w;
} my_t;

double z;

double foo (int i, int j)
{

  if (j)
    {
      switch (i)
	{
	case 0:
	  return 1;
	case 1:
	  return 0;
	case 2:
	  return 3.0;
	}
    }

  if (i == 1)
    {
      my_t u;
      u.v = z;
      u.w = 1;
      z = u.v;
    }
  return z;
}

/* { dg-final { scan-assembler-not {\mth\.flrd\M} } } */
/* { dg-final { scan-assembler-times {\mlw\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfmvp\.d\.x\M} 3 } } */
