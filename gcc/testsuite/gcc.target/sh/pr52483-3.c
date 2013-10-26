/* Check that loads/stores from/to volatile mems utilize indexed addressing
   modes and do not result in redundant sign/zero extensions. */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "@\\(r0," 6 } } */
/* { dg-final { scan-assembler-not "exts|extu" } } */

int
test_00 (volatile char* x, unsigned int y)
{
  return x[y];
}

void
test_100 (volatile char* x, unsigned int y, char z)
{
  x[y] = z;
}

int
test_01 (volatile short* x, unsigned int y)
{
  return x[y];
}

void
test_101 (volatile short* x, unsigned int y, short z)
{
  x[y] = z;
}

int
test_02 (volatile int* x, unsigned int y)
{
  return x[y];
}

int
test_102 (volatile int* x, unsigned int y, int z)
{
  x[y] = z;
}
