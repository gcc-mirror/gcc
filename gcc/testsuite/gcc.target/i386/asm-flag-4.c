/* Test some of the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#define DO(C) \
void f##C(int *y) { char x; asm("" : "=@cc"#C(x)); if (!x) *y = 0; }

DO(b)
DO(l)
DO(z)
DO(be)
DO(ge)
DO(le)

/* { dg-final { scan-assembler "jc" } } */
/* { dg-final { scan-assembler "jl" } } */
/* { dg-final { scan-assembler "je" } } */
/* { dg-final { scan-assembler "jna" } } */
/* { dg-final { scan-assembler "jge" } } */
/* { dg-final { scan-assembler "jle" } } */
