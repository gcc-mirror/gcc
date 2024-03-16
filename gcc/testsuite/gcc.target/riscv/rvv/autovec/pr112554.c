/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 --param=riscv-autovec-preference=fixed-vlmax" } */

int a;
void b() {
  unsigned long c = 18446744073709551612UL;
d:
  --c;
  a ^= c;
  if (c)
    goto d;
}
