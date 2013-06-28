/* Check that loads from volatile mems don't result in redundant sign
   extensions.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-not "exts" } } */

int
test_00 (volatile char* x)
{
  return *x;
}

int
test_01 (volatile short* x)
{
  return *x;
}

int
test_02 (volatile unsigned char* x)
{
  return *x == 0x80;
}

int
test_03 (volatile unsigned short* x)
{
  return *x == 0xFF80;
}
