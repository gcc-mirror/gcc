/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O2 -fzero-call-used-regs=all-gpr" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler-not {\mvsetvli} } } */
/* { dg-final { scan-assembler "li\[ \t\]*t0,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t1,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t2,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a0,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a1,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a2,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a3,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a4,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a5,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a6,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a7,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t3,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t4,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t5,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t6,0" } } */
