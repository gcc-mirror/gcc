/* Check that loads/stores from/to volatile mems don't result in redundant
   sign/zero extensions.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-not "exts|extu" } } */

int
test_00 (volatile char* x)
{
  return *x;
}

void
test_100 (volatile char* x, char y)
{
  *x = y;
}

int
test_01 (volatile short* x)
{
  return *x;
}

void
test_101 (volatile unsigned char* x, unsigned char y)
{
  *x = y;
}

int
test_02 (volatile unsigned char* x)
{
  return *x == 0x80;
}

void
test_102 (volatile short* x, short y)
{
  *x = y;
}

int
test_03 (volatile unsigned short* x)
{
  return *x == 0xFF80;
}

void
test_103 (volatile unsigned short* x, unsigned short y)
{
  *x = y;
}
