/* PR rtl-optimization/92180 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int foo() {
  return __builtin_ia32_rdtsc();
}

/* { dg-final { scan-assembler-not "sal" } } */
