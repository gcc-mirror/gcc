/* PR target/50749: Verify that subsequent pre-decrement addressings
   are generated.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "mov.b\tr\[0-9]\+,@-r\[0-9]\+" 5 { xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "mov.w\tr\[0-9]\+,@-r\[0-9]\+" 5 { xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "mov.l\tr\[0-9]\+,@-r\[0-9]\+" 5 { xfail *-*-*} } } */

char*
test_func_00 (char* p, int c)
{
  *--p = (char)c;
  *--p = (char)c;
  return p;
}

char*
test_func_01 (char* p, int c)
{
  *--p = (char)c;
  *--p = (char)c;
  *--p = (char)c;
  return p;
}

short*
test_func_02 (short* p, int c)
{
  *--p = (short)c;
  *--p = (short)c;
  return p;
}

short*
test_func_03 (short* p, int c)
{
  *--p = (short)c;
  *--p = (short)c;
  *--p = (short)c;
  return p;
}

int*
test_func_04 (int* p, int c)
{
  *--p = c;
  *--p = c;
  return p;
}

int*
test_func_05 (int* p, int c)
{
  *--p = c;
  *--p = c;
  *--p = c;
  return p;
}
