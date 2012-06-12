/* PR target/50749: Verify that post-increment addressing is generated
   inside a loop.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "mov.b\t@r\[0-9]\+\\+,r\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "mov.w\t@r\[0-9]\+\\+,r\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "mov.l\t@r\[0-9]\+\\+,r\[0-9]\+" 1 } } */

int
test_func_00 (char* p, int c)
{
  int r = 0;
  do
  {
    r += *p++;
  } while (--c);
  return r;
}

int
test_func_01 (short* p, int c)
{
  int r = 0;
  do
  {
    r += *p++;
  } while (--c);
  return r;
}

int
test_func_02 (int* p, int c)
{
  int r = 0;
  do
  {
    r += *p++;
  } while (--c);
  return r;
}
