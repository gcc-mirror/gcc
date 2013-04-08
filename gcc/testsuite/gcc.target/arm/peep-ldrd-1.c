/* { dg-do compile } */
/* { dg-require-effective-target arm_prefer_ldrd_strd } */
/* { dg-options "-O2" }  */
int foo(int a, int b, int* p, int *q)
{
  a = p[2] + p[3];
  *q = a;
  *p = a;
  return a;
}
/* { dg-final { scan-assembler "ldrd" } } */
