/* { dg-do compile } */
/* { dg-options "-O2" } */

long long calc (long long a, long long b) {
  return a * b + 5;
}

/* Ensure our return value is set in the r11, r12 pair.  */
/* { dg-final { scan-assembler "r11," } } */
/* { dg-final { scan-assembler "r12," } } */
