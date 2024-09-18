/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zfa -mabi=ilp32d -O2 -g" } */

unsigned int
foo (double a) {
  unsigned int tt = *(unsigned long long *)&a & 0xffff;
  return tt;
}
