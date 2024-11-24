/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_zic64b_zicboz" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zic64b_zicboz" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

// 1x cbo.zero, 7x sd (rv64) or 14x sw (rv32), 1x sh, 1x sb
int
clear_buf_123 (void *p)
{
  p = __builtin_assume_aligned(p, 64);
  __builtin_memset (p, 0, 123);
}

// 2x cbo.zero, 1x addi 64
int
clear_buf_128 (void *p)
{
  p = __builtin_assume_aligned(p, 64);
  __builtin_memset (p, 0, 128);
}

/* { dg-final { scan-assembler-times "cbo\.zero\t" 3 } } */
/* { dg-final { scan-assembler-times "addi\ta\[0-9\]+,a\[0-9\]+,64" 1 } } */
/* { dg-final { scan-assembler-times "sd\t" 7 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "sw\t" 14 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "sh\t" 1 } } */
/* { dg-final { scan-assembler-times "sb\t" 1 } } */
