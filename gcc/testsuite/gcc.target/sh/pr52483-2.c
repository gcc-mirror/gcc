/* Check that loads from volatile mems utilize displacement addressing
   modes and do not result in redundant sign extensions. */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "@\\(5," 2 } } */
/* { dg-final { scan-assembler-times "@\\(10," 2 } } */
/* { dg-final { scan-assembler-times "@\\(20," 2 } } */
/* { dg-final { scan-assembler-times "@\\(40," 2 } } */
/* { dg-final { scan-assembler-times "@\\(44," 2 } } */
/* { dg-final { scan-assembler-not "exts" } } */

int
test_00 (volatile char* x)
{
  return x[5];
}

int
test_01 (volatile short* x)
{
  return x[5];
}

int
test_02 (volatile int* x)
{
  return x[5];
}

long long
test_03 (volatile long long* x)
{
  return x[5];
}

unsigned int
test_04 (volatile unsigned char* x)
{
  return x[5];
}

unsigned int
test_05 (volatile unsigned short* x)
{
  return x[5];
}
 
unsigned int
test_06 (volatile unsigned int* x)
{
  return x[5];
}

unsigned long long
test_07 (volatile unsigned long long* x)
{
  return x[5];
}
