/* Check that for a field that starts at bit 0, feeding a sCC insn,
   btst/btstq is used. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler-not "\tmov" } } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler-times "\tbtstq" 6 } } */
bool b7(int a) {
  return (a & 127) == 0;
}

int i7n(int a) {
  return (a & 127) != 0;
}

bool b4(int a) {
  return (a & 15) == 0;
}

int i4n(int a) {
  return (a & 15) != 0;
}

bool b4n(int a) {
  return (a & 15) != 0;
}

int i4(int a) {
  return (a & 15) == 0;
}
