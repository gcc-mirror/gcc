/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 fool(unsigned long x) { return -(__int128)x; }

/* { dg-final { scan-assembler "sbb" } } */
/* { dg-final { scan-assembler-not "adc" } } */
