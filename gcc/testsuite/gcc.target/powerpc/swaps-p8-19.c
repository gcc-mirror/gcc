/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -ftree-vectorize -mdejagnu-cpu=power8 -mvsx -ffast-math -fvect-cost-model=unlimited" } */

/* This tests special handling for various uses of xxpermdi, other than
   to perform doubleword swaps.  */

void foo (_Complex double *self, _Complex double *a, _Complex double *b,
	  int a1, int a2)
{
  int i, j;
  for (i = 0; i < a1; ++i)
    for (j = 0; j < a2; ++j)
      self[i] = self[i] + a[i,j] * b[j];
}

/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,0" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,1" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,2" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,3" 1 } } */
