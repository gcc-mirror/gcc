/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz"} } */

long primitiveSemantics_return_0_imm_00(long a, long b) {
  return a == 0 ? 0 : 3;
}

long primitiveSemantics_return_0_imm_01(long a, long b) {
  return a != 0 ? 0 : 3;
}

long primitiveSemantics_return_0_imm_02(long a, long b) {
  return a == 0 ? 3 : 0;
}

long primitiveSemantics_return_0_imm_03(long a, long b) {
  return a != 0 ? 3 : 0;
}

long primitiveSemantics_return_0_imm_04(long a, long b) {
  if (a)
    b = 0;
  else
    b = 3;
  return b;
}

long primitiveSemantics_return_0_imm_05(long a, long b) {
  if (!a)
    b = 0;
  else
    b = 3;
  return b;
}

int primitiveSemantics_return_0_imm_06(int a, int b) { return a == 0 ? 0 : 3; }

int primitiveSemantics_return_0_imm_07(int a, int b) { return a != 0 ? 0 : 3; }

int primitiveSemantics_return_0_imm_08(int a, int b) { return a == 0 ? 3 : 0; }

int primitiveSemantics_return_0_imm_09(int a, int b) { return a != 0 ? 3 : 0; }

int primitiveSemantics_return_0_imm_10(int a, int b) {
  if (a)
    b = 0;
  else
    b = 3;
  return b;
}

int primitiveSemantics_return_0_imm_11(int a, int b) {
  if (!a)
    b = 0;
  else
    b = 3;
  return b;
}

/* { dg-final { scan-assembler-times {\mczero\.eqz\M} 6 } } */
/* { dg-final { scan-assembler-times {\mczero\.nez\M} 6 } } */
/* { dg-final { scan-assembler-not {\mbeq\M} } } */
/* { dg-final { scan-assembler-not {\mbne\M} } } */
