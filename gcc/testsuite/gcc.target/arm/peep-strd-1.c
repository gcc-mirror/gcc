/* { dg-do compile } */
/* { dg-require-effective-target arm_prefer_ldrd_strd } */
/* { dg-options "-O2" }  */
void foo(int a, int b, int* p)
{
  p[2] = a;
  p[3] = b;
}
/* { dg-final { scan-assembler "strd" } } */
