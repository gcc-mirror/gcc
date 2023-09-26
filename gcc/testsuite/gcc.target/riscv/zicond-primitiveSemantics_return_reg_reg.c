/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz"} } */

long primitiveSemantics_return_reg_reg_00(long a, long b, long c) {
  return a == 0 ? c : b;
}

long primitiveSemantics_return_reg_reg_01(long a, long b, long c) {
  return a != 0 ? c : b;
}

long primitiveSemantics_return_reg_reg_02(long a, long b, long c) {
  return a == 0 ? b : c;
}

long primitiveSemantics_return_reg_reg_03(long a, long b, long c) {
  return a != 0 ? b : c;
}

long primitiveSemantics_return_reg_reg_04(long a, long b, long c) {
  if (a)
    b = c;
  return b;
}

long primitiveSemantics_return_reg_reg_05(long a, long b, long c) {
  if (!a)
    b = c;
  return b;
}

int primitiveSemantics_return_reg_reg_06(int a, int b, int c) {
  return a == 0 ? c : b;
}

int primitiveSemantics_return_reg_reg_07(int a, int b, int c) {
  return a != 0 ? c : b;
}

int primitiveSemantics_return_reg_reg_08(int a, int b, int c) {
  return a == 0 ? b : c;
}

int primitiveSemantics_return_reg_reg_09(int a, int b, int c) {
  return a != 0 ? b : c;
}

int primitiveSemantics_return_reg_reg_10(int a, int b, int c) {
  if (a)
    b = c;
  return b;
}

int primitiveSemantics_return_reg_reg_11(int a, int b, int c) {
  if (!a)
    b = c;
  return b;
}

/* { dg-final { scan-assembler-times "czero.eqz" 12 } } */
/* { dg-final { scan-assembler-times "czero.nez" 12 } } */
/* { dg-final { scan-assembler-not "beq" } } */
/* { dg-final { scan-assembler-not "bne" } } */
