/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0"} } */

long primitiveSemantics_00(long a, long b) { return a == 0 ? 0 : b; }

long primitiveSemantics_01(long a, long b) { return a != 0 ? 0 : b; }

long primitiveSemantics_02(long a, long b) { return a == 0 ? b : 0; }

long primitiveSemantics_03(long a, long b) { return a != 0 ? b : 0; }

long primitiveSemantics_04(long a, long b) {
  if (a)
    b = 0;
  return b;
}

long primitiveSemantics_05(long a, long b) {
  if (!a)
    b = 0;
  return b;
}

int primitiveSemantics_06(int a, int b) { return a == 0 ? 0 : b; }

int primitiveSemantics_07(int a, int b) { return a != 0 ? 0 : b; }

int primitiveSemantics_08(int a, int b) { return a == 0 ? b : 0; }

int primitiveSemantics_09(int a, int b) { return a != 0 ? b : 0; }

int primitiveSemantics_10(int a, int b) {
  if (a)
    b = 0;
  return b;
}

int primitiveSemantics_11(int a, int b) {
  if (!a)
    b = 0;
  return b;
}

/* { dg-final { scan-assembler-times "czero.eqz" 6 } } */
/* { dg-final { scan-assembler-times "czero.nez" 6 } } */
/* { dg-final { scan-assembler-not "beq" } } */
/* { dg-final { scan-assembler-not "bne" } } */
