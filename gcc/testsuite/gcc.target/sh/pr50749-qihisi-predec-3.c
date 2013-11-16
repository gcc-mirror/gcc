/* PR target/50749: Verify that pre-decrement addressing is generated
   inside a loop.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "mov.b\tr\[0-9]\+,@-r\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "mov.w\tr\[0-9]\+,@-r\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "mov.l\tr\[0-9]\+,@-r\[0-9]\+" 1 } } */

char*
test_func_00 (char* p, int c, int x)
{
  do
  {
    *--p = (char)x;
  } while (--c);
  return p;
}

short*
test_func_01 (short* p, int c, int x)
{
  do
  {
    *--p = (short)x;
  } while (--c);
  return p;
}

int*
test_func_02 (int* p, int c, int x)
{
  do
  {
    *--p = x;
  } while (--c);
  return p;
}
