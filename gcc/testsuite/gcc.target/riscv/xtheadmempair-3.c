/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Oz" "-Os" "-flto" } } */
/* { dg-options "-march=rv64gc_xtheadmempair -mtune=thead-c906" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadmempair -mtune=thead-c906" { target { rv32 } } } */

#include <inttypes.h>

#if __riscv_xlen == 32
typedef uint32_t xlen_t;
#else
typedef uint64_t xlen_t;
#endif

void foo (xlen_t, xlen_t, xlen_t, xlen_t, xlen_t, xlen_t, xlen_t, xlen_t);
void bar (xlen_t, xlen_t, xlen_t, xlen_t, xlen_t, xlen_t, xlen_t, xlen_t);

void baz (xlen_t a, xlen_t b, xlen_t c, xlen_t d, xlen_t e, xlen_t f, xlen_t g, xlen_t h)
{
  foo (a, b, c, d, e, f, g, h);
  /* RV64: We don't use 0(sp), therefore we can only get 3 mempairs.  */
  /* RV32: We don't use 0(sp)-8(sp), therefore we can only get 2 mempairs.  */
  bar (a, b, c, d, e, f, g, h);
}

/* { dg-final { scan-assembler-times "th.ldd\t" 3 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.sdd\t" 3 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "th.lwd\t" 2 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "th.swd\t" 2 { target { rv32 } } } } */
