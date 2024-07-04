/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d -mbranch-cost=4" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f -mbranch-cost=4" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Os" "-Oz"} } */

long primitiveSemantics_compare_imm_return_reg_reg_00(long a, long b, long c) {
  return a == 2 ? c : b;
}

long primitiveSemantics_compare_imm_return_reg_reg_01(long a, long b, long c) {
  return a != 2 ? c : b;
}

long primitiveSemantics_compare_imm_return_reg_reg_02(long a, long b, long c) {
  return a == 2 ? b : c;
}

long primitiveSemantics_compare_imm_return_reg_reg_03(long a, long b, long c) {
  return a != 2 ? b : c;
}

long primitiveSemantics_compare_imm_return_reg_reg_04(long a, long b, long c) {
  if (a == 2)
    b = c;
  return b;
}

long primitiveSemantics_compare_imm_return_reg_reg_05(long a, long b, long c) {
  if (!(a == 2))
    b = c;
  return b;
}

int primitiveSemantics_compare_imm_return_reg_reg_06(int a, int b, int c) {
  return a == 2 ? c : b;
}

int primitiveSemantics_compare_imm_return_reg_reg_07(int a, int b, int c) {
  return a != 2 ? c : b;
}

int primitiveSemantics_compare_imm_return_reg_reg_08(int a, int b, int c) {
  return a == 2 ? b : c;
}

int primitiveSemantics_compare_imm_return_reg_reg_09(int a, int b, int c) {
  return a != 2 ? b : c;
}

int primitiveSemantics_compare_imm_return_reg_reg_10(int a, int b, int c) {
  if ((a == 2))
    b = c;
  return b;
}

int primitiveSemantics_compare_imm_return_reg_reg_11(int a, int b, int c) {
  if (!(a == 2))
    b = c;
  return b;
}

/* { dg-final { scan-assembler-times {\mczero.eqz\M} 12 } } */
/* { dg-final { scan-assembler-times {\mczero.nez\M} 12 } } */
/* { dg-final { scan-assembler-not {\mbeq\M} } } */
/* { dg-final { scan-assembler-not {\mbne\M} } } */
