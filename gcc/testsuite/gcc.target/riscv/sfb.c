//* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32d -mtune=sifive-7-series" } */

int f1(unsigned int x, unsigned int y, unsigned int z)
{
  return ((x & 1) == 0) ? y : z ^ y;
}

int f2(unsigned int x, unsigned int y, unsigned int z)
{
  return ((x & 1) != 0) ? z ^ y : y;
}

int f3(unsigned int x, unsigned int y, unsigned int z)
{
  return ((x & 1) == 0) ? y : z | y;
}

int f4(unsigned int x, unsigned int y, unsigned int z)
{
  return ((x & 1) != 0) ? z | y : y;
}
/* { dg-final { scan-assembler-times "bne" 4 } } */
/* { dg-final { scan-assembler-times "movcc" 4 } } */
