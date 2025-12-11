/* Verify optimization for mulh.wu, which can reduce insns.  */
/* { dg-do compile } */
/* { dg-options "-O3" } */

int
test (unsigned int a)
{
  return a / 3;
}

/* { dg-final { scan-assembler {\tmulh.wu\t} } } */
