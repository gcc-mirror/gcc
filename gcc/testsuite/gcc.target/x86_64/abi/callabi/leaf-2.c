/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -mabi=sysv" } */

extern int glb1, gbl2, gbl3;

__attribute__ ((ms_abi))
int foo (void)
{
  int r = 1;
  int i, j, k;
  for (i = 0; i < glb1; i++)
  {
     r *= (i + 1);
     for (j = gbl2; j > 0; --j)
       {
	 for (k = 0; k < gbl3; k++)
	   r += (i + k * j);
       }
  }

  return r;
}

/* { dg-final { scan-assembler-not "%rsp" } } */

