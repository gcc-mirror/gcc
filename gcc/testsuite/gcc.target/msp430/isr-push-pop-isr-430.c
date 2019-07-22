/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430x*" "-mlarge" } { "" } } */
/* { dg-options "-mcpu=msp430" } */
/* { dg-final { scan-assembler "PUSH\tR11" } } */
/* { dg-final { scan-assembler-not "PUSH\tR10" } } */

void __attribute__((noinline)) callee (void);

void __attribute__((interrupt))
isr (void)
{
  callee();
}
