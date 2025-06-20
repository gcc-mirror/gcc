/* Reduced from SPEC2017 blender: node_texture_util.c.
   The conditional function call was tripping mode switching state machine */

/* { dg-do compile  { target { rv64 && { ! riscv_abi_e } } } } */
/* { dg-options " -Ofast -march=rv64gcv_zvl256b -ftree-vectorize -mrvv-vector-bits=zvl" } */

void *a;
float *b;
short c;
void d();
void e() {
  if (a)
    d();
  if (c) {
    b[0] = b[0] * 0.5f + 0.5f;
    b[1] = b[1] * 0.5f + 0.5f;
  }
}

/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
