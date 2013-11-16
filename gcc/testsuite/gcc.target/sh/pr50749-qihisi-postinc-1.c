/* PR target/50749: Verify that post-increment addressing is generated.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "mov.b\t@r\[0-9]\+\\+,r\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "mov.w\t@r\[0-9]\+\\+,r\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "mov.l\t@r\[0-9]\+\\+,r\[0-9]\+" 1 } } */

char*
test_func_00 (char* p, int* x)
{
  int r = 0;
  r += *p++;
  *x = r;
  return p;
}

short*
test_func_01 (short* p, int* x)
{
  int r = 0;
  r += *p++;
  *x = r;
  return p;
}

int*
test_func_02 (int* p, int* x)
{
  int r = 0;
  r += *p++;
  *x = r;
  return p;
}

