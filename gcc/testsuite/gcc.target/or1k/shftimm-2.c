/* { dg-do compile } */
/* { dg-options "-mshftimm -O2" } */

unsigned int shift6 (unsigned int a) {
  return a >> 6;
}

/* { dg-final { scan-assembler "l.srli" } } */
