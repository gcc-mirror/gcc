/* { dg-do compile } */
/* { dg-options "-march=rv32i -mabi=ilp32 -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i -mabi=lp64 -O" { target { rv64 } } } */

/* 1) bf -> sf (call      __extendbfsf2)  */
/* 2) sf -> bf (call      __truncsfbf2)  */
__attribute__ ((noinline)) __bf16 add (__bf16 a, __bf16 b) { return a + b; }

__bf16 test(__bf16 a, __bf16 b) { return add (a, b); }

/* { dg-final { scan-assembler-times "call\t__extendbfsf2" 2 } } */
/* { dg-final { scan-assembler-times "call\t__truncsfbf2" 1 } } */
