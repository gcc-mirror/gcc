/* { dg-do compile } */
/* { dg-options "-O2" } */

static int __attribute__ ((__noclone__, __noinline__))
reg_args (int j1, int j2, int j3, int j4, int j5, int j6, int j7, int j8)
{
  return j1 + j2 + j3 + j4 + j5 + j6 + j7 + j8;
}

int __attribute__ ((__noclone__, __noinline__))
stack_args (int j1, int j2, int j3, int j4, int j5, int j6, int j7, int j8,
	    int j9)
{
  if (j9 == 0)
    return 0;
  return reg_args (j1, j2, j3, j4, j5, j6, j7, j8);
}

/* { dg-final { scan-assembler-not {(?n)^\s+bl\s} } } */
