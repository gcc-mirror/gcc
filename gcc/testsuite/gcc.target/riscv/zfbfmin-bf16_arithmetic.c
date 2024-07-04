/* { dg-do compile } */
/* { dg-options "-march=rv32i_zfbfmin -mabi=ilp32f -mcmodel=medany -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i_zfbfmin -mabi=lp64f -mcmodel=medany -O" { target { rv64 } } } */

/* 1) bf -> sf          fcvt.s.bf16  */
/* 2) sf1 [+|-|*|/] sf2 f[add|sub|mul|div].s  */
/* 3) sf -> bf          fcvt.bf16.s  */
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

/* { dg-final { scan-assembler-times "fcvt.s.bf16" 14 } } */
/* { dg-final { scan-assembler-times "fcvt.bf16.s" 10 } } */

/* { dg-final { scan-assembler-not "call" } } */
