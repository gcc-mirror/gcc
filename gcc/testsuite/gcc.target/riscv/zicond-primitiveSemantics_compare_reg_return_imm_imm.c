/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d -mbranch-cost=3" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f -mbranch-cost=3" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Os" "-Oz"} } */

long primitiveSemantics_compare_reg_return_imm_imm_00(long a, long b, long c) {
  return a == c ? 7 : 4;
}

long primitiveSemantics_compare_reg_return_imm_imm_01(long a, long b, long c) {
  return a != c ? 7 : 4;
}

long primitiveSemantics_compare_reg_return_imm_imm_02(long a, long b, long c) {
  return a == c ? 7 : 4;
}

long primitiveSemantics_compare_reg_return_imm_imm_03(long a, long b, long c) {
  return a != c ? 7 : 4;
}

long primitiveSemantics_compare_reg_return_imm_imm_04(long a, long b, long c) {
  if (a == c)
    b = 7;
  else
    b = 4;
  return b;
}

long primitiveSemantics_compare_reg_return_imm_imm_05(long a, long b, long c) {
  if (!(a == c))
    b = 7;
  else
    b = 4;
  return b;
}

int primitiveSemantics_compare_reg_return_imm_imm_06(int a, int b, int c) {
  return a == c ? 7 : 4;
}

int primitiveSemantics_compare_reg_return_imm_imm_07(int a, int b, int c) {
  return a != c ? 7 : 4;
}

int primitiveSemantics_compare_reg_return_imm_imm_08(int a, int b, int c) {
  return a == c ? 7 : 4;
}

int primitiveSemantics_compare_reg_return_imm_imm_09(int a, int b, int c) {
  return a != c ? 7 : 4;
}

int primitiveSemantics_compare_reg_return_imm_imm_10(int a, int b, int c) {
  if ((a == c))
    b = 7;
  else
    b = 4;
  return b;
}

int primitiveSemantics_compare_reg_return_imm_imm_11(int a, int b, int c) {
  if (!(a == c))
    b = 7;
  else
    b = 4;
  return b;
}

/* { dg-final { scan-assembler-times {\mczero.eqz\M} 6 } } */
/* { dg-final { scan-assembler-times {\mczero.nez\M} 6 } } */
/* { dg-final { scan-assembler-not {\mbeq\M} } } */
/* { dg-final { scan-assembler-not {\mbne\M} } } */
