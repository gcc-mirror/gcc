/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d -mbranch-cost=3" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f -mbranch-cost=3" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Os" "-Oz"} } */

long primitiveSemantics_compare_reg_return_imm_reg_00(long a, long b, long c) {
  return a == c ? 10 : b;
}

long primitiveSemantics_compare_reg_return_imm_reg_01(long a, long b, long c) {
  return a != c ? 10 : b;
}

long primitiveSemantics_compare_reg_return_imm_reg_02(long a, long b, long c) {
  return a == c ? b : 10;
}

long primitiveSemantics_compare_reg_return_imm_reg_03(long a, long b, long c) {
  return a != c ? b : 10;
}

long primitiveSemantics_compare_reg_return_imm_reg_04(long a, long b, long c) {
  if (a == c)
    b = 10;
  return b;
}

long primitiveSemantics_compare_reg_return_imm_reg_05(long a, long b, long c) {
  if (!(a == c))
    b = 10;
  return b;
}

int primitiveSemantics_compare_reg_return_imm_reg_06(int a, int b, int c) {
  return a == c ? 10 : b;
}

int primitiveSemantics_compare_reg_return_imm_reg_07(int a, int b, int c) {
  return a != c ? 10 : b;
}

int primitiveSemantics_compare_reg_return_imm_reg_08(int a, int b, int c) {
  return a == c ? b : 10;
}

int primitiveSemantics_compare_reg_return_imm_reg_09(int a, int b, int c) {
  return a != c ? b : 10;
}

int primitiveSemantics_compare_reg_return_imm_reg_10(int a, int b, int c) {
  if ((a == c))
    b = 10;
  return b;
}

int primitiveSemantics_compare_reg_return_imm_reg_11(int a, int b, int c) {
  if (!(a == c))
    b = 10;
  return b;
}

/* { dg-final { scan-assembler-times {\mczero.eqz\M} 6 } } */
/* { dg-final { scan-assembler-times {\mczero.nez\M} 6 } } */
/* { dg-final { scan-assembler-not {\mbeq\M} } } */
/* { dg-final { scan-assembler-not {\mbne\M} } } */
