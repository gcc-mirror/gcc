/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz"} } */

long primitiveSemantics_return_imm_reg_00(long a, long b) {
  return a == 0 ? 1 : b;
}

long primitiveSemantics_return_imm_reg_01(long a, long b) {
  return a != 0 ? 1 : b;
}

long primitiveSemantics_return_imm_reg_02(long a, long b) {
  return a == 0 ? b : 1;
}

long primitiveSemantics_return_imm_reg_03(long a, long b) {
  return a != 0 ? b : 1;
}

long primitiveSemantics_return_imm_reg_04(long a, long b) {
  if (a)
    b = 1;
  return b;
}

long primitiveSemantics_return_imm_reg_05(long a, long b) {
  if (!a)
    b = 1;
  return b;
}

int primitiveSemantics_return_imm_reg_06(int a, int b) {
  return a == 0 ? 1 : b;
}

int primitiveSemantics_return_imm_reg_07(int a, int b) {
  return a != 0 ? 1 : b;
}

int primitiveSemantics_return_imm_reg_08(int a, int b) {
  return a == 0 ? b : 1;
}

int primitiveSemantics_return_imm_reg_09(int a, int b) {
  return a != 0 ? b : 1;
}

int primitiveSemantics_return_imm_reg_10(int a, int b) {
  if (a)
    b = 1;
  return b;
}

int primitiveSemantics_return_imm_reg_11(int a, int b) {
  if (!a)
    b = 1;
  return b;
}

/* { dg-final { scan-assembler-times {\mczero\.eqz\M} 6 } } */
/* { dg-final { scan-assembler-times {\mczero\.nez\M} 6 } } */
/* { dg-final { scan-assembler-not {\mbeq\M} } } */
/* { dg-final { scan-assembler-not {\mbne\M} } } */
