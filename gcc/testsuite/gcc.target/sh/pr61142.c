/* Check that @(r0,rm),rn insns load into r0.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-times "mov.b\t@\\(r0,r\[0123456789\]\\),r0" 1 } }  */
/* { dg-final { scan-assembler-times "mov.w\t@\\(r0,r\[0123456789\]\\),r0" 1 } }  */
/* { dg-final { scan-assembler-times "mov.l\t@\\(r0,r\[0123456789\]\\),r0" 1 } }  */

int
test_00 (const char* x, int a, int b, int c)
{
  if (x[a] == 92)
    return b;
  return c;
}

int
test_01 (const short* x, int a, int b, int c)
{
  if (x[a] == 92)
    return b;
  return c;
}

int
test_02 (const int* x, int a, int b, int c)
{
  if (x[a] == 92)
    return b;
  return c;
}
