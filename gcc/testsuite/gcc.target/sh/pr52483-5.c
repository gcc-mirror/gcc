/* Check that loads from volatile mems utilize post-increment addressing
   modes and do not result in redundant sign extensions. */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "@r\[0-9\]\+\\+," 3 } } */
/* { dg-final { scan-assembler-not "exts" } } */

volatile char*
test_00 (volatile char* x)
{
  int xx = *x++;
  return x;
}

volatile short*
test_01 (volatile short* x)
{
  int xx = *x++;
  return x;
}

volatile int*
test_02 (volatile int* x)
{
  int xx = *x++;
  return x;
}
