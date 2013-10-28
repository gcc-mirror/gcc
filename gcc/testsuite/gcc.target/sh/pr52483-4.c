/* Check that loads/stores from/to volatile floating point mems utilize
   indexed addressing modes. */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-times "@\\(r0," 2 } } */

float
test_00 (volatile float* x, unsigned int y)
{
  return x[y];
}

void
test_100 (volatile float* x, unsigned int y, float z)
{
  x[y] = z;
}
