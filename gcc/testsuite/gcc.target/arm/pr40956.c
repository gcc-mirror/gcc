/* { dg-options "-Os -fpic" }  */
/* { dg-require-effective-target fpic } */
/* Make sure the constant "0" is loaded into register only once.  */
/* { dg-final { scan-assembler-times "mov\[\\t \]*r., #0" 1 } } */

int foo(int p, int* q)
{
  if (p!=9)
    *q = 0;
  else
    *(q+1) = 0;
  return 3;
}
