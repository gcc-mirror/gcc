/* Check that loads/stores from/to volatile mems utilize displacement
   addressing modes and do not result in redundant sign/zero extensions. */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "@\\(5," 4 } } */
/* { dg-final { scan-assembler-times "@\\(10," 4 } } */
/* { dg-final { scan-assembler-times "@\\(20," 4 } } */
/* { dg-final { scan-assembler-times "@\\(40," 4 } } */
/* { dg-final { scan-assembler-times "@\\(44," 4 } } */
/* { dg-final { scan-assembler-not "exts" } } */
/* { dg-final { scan-assembler-times "extu|movu" 2 } } */

int
test_00 (volatile char* x)
{
  return x[5];
}

void
test_100 (volatile char* x, char y)
{
  x[5] = y;
}

int
test_01 (volatile short* x)
{
  return x[5];
}

void
test_101 (volatile short* x, short y)
{
  x[5] = y;
}

int
test_02 (volatile int* x)
{
  return x[5];
}

void
test_102 (volatile int* x, int y)
{
  x[5] = y;
}

long long
test_03 (volatile long long* x)
{
  return x[5];
}

void
test_103 (volatile long long* x, long long y)
{
  x[5] = y;
}

unsigned int
test_04 (volatile unsigned char* x)
{
  // expected 1x extu.b or movu.b
  return x[5];
}

void
test_104 (volatile unsigned char* x, unsigned char y)
{
  x[5] = y;
}

unsigned int
test_05 (volatile unsigned short* x)
{
  // expected 1x extu.w or movu.w
  return x[5];
}

void
test_105 (volatile unsigned short* x, unsigned short y)
{
  x[5] = y;
}
 
unsigned int
test_06 (volatile unsigned int* x)
{
  return x[5];
}

void
test_106 (volatile unsigned int* x, unsigned int y)
{
  x[5] = y;
}

unsigned long long
test_07 (volatile unsigned long long* x)
{
  return x[5];
}

void
test_107 (volatile unsigned long long* x, unsigned long long y)
{
  x[5] = y;
}
