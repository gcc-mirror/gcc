/* Check that loads from volatile mems utilize indexed addressing
   modes and do not result in redundant sign extensions. */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "@\\(r0," 3 } } */
/* { dg-final { scan-assembler-not "exts" } } */

int
test_00 (volatile char* x, unsigned int y)
{
  return x[y];
}

int
test_01 (volatile short* x, unsigned int y)
{
  return x[y];
}

int
test_02 (volatile int* x, unsigned int y)
{
  return x[y];
}
