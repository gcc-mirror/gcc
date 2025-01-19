/* { dg-do run } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64d" { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv32gc_zbs -mabi=ilp32" { target { riscv32*-*-* } } } */
#include <stdint-gcc.h>
int32_t a, b;
int32_t d;
int64_t f = 695372830942;
int main() {
  d = 0;
  for (; d < 1; d = 1)
    --f;
  d |= b = f;
  int64_t h = d;
  a = h >> 40;
  if (a != -1)
    __builtin_abort ();
  __builtin_exit (0);
}
