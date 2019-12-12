/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } { "" } } */
/* { dg-final { scan-assembler "PUSHM.*#5" } } */
/* { dg-final { scan-assembler-not "PUSHM.*#12" } } */

void __attribute__((noinline)) callee (void);

void __attribute__((interrupt))
isr (void)
{
  callee();
}
