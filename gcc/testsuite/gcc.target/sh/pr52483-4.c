/* Check that loads/stores from/to volatile floating point mems utilize
   indexed addressing modes. */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O1" } */
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
