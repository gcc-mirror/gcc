/* Slightly modified copy of gcc.target/arm/pr40956.c.  */
/* { dg-options "-Os" }  */
/* Make sure the constant "6" is loaded into register only once.  */
/* { dg-final { scan-assembler-times "\tli.*6" 1 } } */

int foo(int p, int* q)
{
  if (p!=9)
    *q = 6;
  else
    *(q+1) = 6;
  return 3;
}
