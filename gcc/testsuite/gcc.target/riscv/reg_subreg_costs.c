/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_zba" } */

#include <stdint-gcc.h>
void foo(uint32_t a, uint64_t *b_ptr, uint64_t b, uint64_t *c_ptr, uint64_t c)
{
  uint64_t x = a;
  *b_ptr = b + x;
  *c_ptr = c + x;
}

/* { dg-final { scan-assembler-not "\\szext.w\\s" } } */

