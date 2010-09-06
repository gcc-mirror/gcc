/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "mov\tr1, r\[1-9\]" } } */

int foo();
long long bar22()
{
  int result = foo();
  return result;
}
