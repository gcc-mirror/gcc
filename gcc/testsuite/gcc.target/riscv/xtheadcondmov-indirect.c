/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadcondmov" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadcondmov" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

/* addi aX, aX, -1000
   li aX, 10
   th.mvnez aX, aX, aX  */
int ConEmv_imm_imm_reg(int x, int y)
{
  if (x == 1000)
    return 10;
  return y;
}

/* addi	aX, aX, -1000
   th.mveqz aX, aX, aX  */
int ConEmv_imm_reg_reg(int x, int y, int z)
{
  if (x == 1000)
    return y;
  return z;
}

/* sub aX, aX, aX
   li aX, 10
   th.mvnez aX, aX, aX  */
int ConEmv_reg_imm_reg(int x, int y, int z)
{
  if (x == y)
    return 10;
  return z;
}

/* sub aX, aX, aX
   th.mveqz aX, aX, aX  */
int ConEmv_reg_reg_reg(int x, int y, int z, int n)
{
  if (x == y)
    return z;
  return n;
}

/* addi aX, aX, -1000
   li aX, 9998336
   addi aX, aX, 1664
   th.mveqz aX, aX, aX  */
int ConNmv_imm_imm_reg(int x, int y)
{
  if (x != 1000)
    return 10000000;
  return y;
}

/* addi aX, aX, 1000
   th.mvnez aX, aX, aX  */
int ConNmv_imm_reg_reg(int x, int y, int z)
{
  if (x != 1000)
    return y;
  return z;
}

/* sub aX, aX, aX
   li aX, 10
   th.mveqz aX, aX, aX  */
int ConNmv_reg_imm_reg(int x, int y, int z)
{
  if (x != y)
    return 10;
  return z;
}

/* sub aX, aX, aX
   th.mvnez aX, aX, aX  */
int ConNmv_reg_reg_reg(int x, int y, int z, int n)
{
  if (x != y)
    return z;
  return n;
}

/* { dg-final { scan-assembler-times "addi\t" 5 } } */
/* { dg-final { scan-assembler-times "li\t" 4 } } */
/* { dg-final { scan-assembler-times "sub\t" 4 } } */
/* { dg-final { scan-assembler-times "th.mveqz\t" 4 } } */
/* { dg-final { scan-assembler-times "th.mvnez\t" 4 } } */
