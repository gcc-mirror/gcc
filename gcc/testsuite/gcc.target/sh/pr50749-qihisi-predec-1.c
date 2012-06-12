/* PR target/50749: Verify that pre-decrement addressing is generated.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "mov.b\tr\[0-9]\+,@-r\[0-9]\+" 1 { xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "mov.w\tr\[0-9]\+,@-r\[0-9]\+" 1 { xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "mov.l\tr\[0-9]\+,@-r\[0-9]\+" 1 { xfail *-*-*} } } */

char*
test_func_00 (char* p, int c)
{
  *--p = (char)c;
  return p;
}

short*
test_func_01 (short* p, int c)
{
  *--p = (short)c;
  return p;
}

int*
test_func_02 (int* p, int c)
{
  *--p = c;
  return p;
}

