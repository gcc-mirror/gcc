/* { dg-do compile } */
/* { dg-options "-march=rv32i -mabi=ilp32 -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i -mabi=lp64 -O" { target { rv64 } } } */

/* 1) bf -> sf          (call      __extendbfsf2)  */
/* 2) sf1 [+|-|*|/] sf2 (call      __[add|sub|mul|div]sf3)  */
/* 3) sf -> bf          (call      __truncsfbf2)  */
extern __bf16 bf;
extern __bf16 bf1;
extern __bf16 bf2;

void bf_add_bf () { bf = bf1 + bf2; }

void bf_sub_bf () { bf = bf1 - bf2; }

void bf_mul_bf () { bf = bf1 * bf2; }

void bf_div_bf () { bf = bf1 / bf2; }

void bf_add_const () { bf = bf1 + 3.14f; }

void const_sub_bf () { bf = 3.14f - bf2; }

void bf_mul_const () { bf = bf1 * 3.14f; }

void const_div_bf () { bf = 3.14f / bf2; }

void bf_inc () { ++bf; }

void bf_dec () { --bf; }

/* { dg-final { scan-assembler-times "call\t__extendbfsf2" 16 } } */
/* { dg-final { scan-assembler-times "call\t__truncsfbf2" 10 } } */
/* { dg-final { scan-assembler-times "call\t__addsf3" 3 } } */
/* { dg-final { scan-assembler-times "call\t__subsf3" 3 } } */
/* { dg-final { scan-assembler-times "call\t__mulsf3" 2 } } */
/* { dg-final { scan-assembler-times "call\t__divsf3" 2 } } */

/* { dg-final { scan-assembler-not "call\t__addbf3" } } */
/* { dg-final { scan-assembler-not "call\t__subbf3" } } */
/* { dg-final { scan-assembler-not "call\t__mulbf3" } } */
/* { dg-final { scan-assembler-not "call\t__divbf3" } } */
