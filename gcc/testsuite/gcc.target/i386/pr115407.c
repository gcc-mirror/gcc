/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -mcmodel=large -mavx512bw" } */
__attribute__((__vector_size__(64))) char v;

void foo() {
  v = v | v << 7;
}

/* { dg-final { scan-assembler "vpternlog.*1to16" } } */
