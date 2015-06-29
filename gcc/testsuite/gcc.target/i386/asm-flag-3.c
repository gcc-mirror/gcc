/* Test some of the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#define DO(C) \
void f##C(int *y) { char x; asm("" : "=@cc"#C(x)); if (!x) *y = 0; }

DO(a)
DO(c)
DO(e)
DO(g)
DO(o)
DO(p)
DO(s)

/* { dg-final { scan-assembler "ja" } } */
/* { dg-final { scan-assembler "jc" } } */
/* { dg-final { scan-assembler "je" } } */
/* { dg-final { scan-assembler "jg" } } */
/* { dg-final { scan-assembler "jo" } } */
/* { dg-final { scan-assembler "jp" } } */
/* { dg-final { scan-assembler "js" } } */
