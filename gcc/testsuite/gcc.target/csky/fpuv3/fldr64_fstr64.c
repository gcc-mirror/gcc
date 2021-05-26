/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O1 -mfpu=fpv3" } */

double fldr64 (double *pd, int index)
{
  return pd[index];
}

/* { dg-final { scan-assembler "fldr.64" } } */

void fstr64 (double *pd, int index, double d)
{
  pd[index] = d;
}

/* { dg-final { scan-assembler "fstr.64" } } */

