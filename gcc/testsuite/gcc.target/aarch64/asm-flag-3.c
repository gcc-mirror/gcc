/* Test some of the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#define DO(C) \
void f##C(void) { char x; asm("" : "=@cc"#C(x)); if (!x) asm(""); asm(""); }

DO(ne)
DO(eq)
DO(cs)
DO(cc)
DO(hs)
DO(lo)
DO(mi)
DO(pl)
DO(vs)
DO(vc)
DO(hi)
DO(ls)
DO(ge)
DO(lt)
DO(gt)
DO(le)

/* { dg-final { scan-assembler "bne" } } */
/* { dg-final { scan-assembler "beq" } } */
/* { dg-final { scan-assembler "bcs" } } */
/* { dg-final { scan-assembler "bcc" } } */
/* { dg-final { scan-assembler "bmi" } } */
/* { dg-final { scan-assembler "bpl" } } */
/* { dg-final { scan-assembler "bvs" } } */
/* { dg-final { scan-assembler "bvc" } } */
/* { dg-final { scan-assembler "bhi" } } */
/* { dg-final { scan-assembler "bls" } } */
/* { dg-final { scan-assembler "bge" } } */
/* { dg-final { scan-assembler "blt" } } */
/* { dg-final { scan-assembler "bgt" } } */
/* { dg-final { scan-assembler "ble" } } */
