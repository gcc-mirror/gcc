/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-options "-O2 -mcpu=power5" } */
/* { dg-final { scan-assembler-times "nop" 3 } } */

/* Test generation of nops in load hit store situation.  Make sure enough nop
   insns are generated to move the load to a new dispatch group.  With the
   simple stw/lwz pair below, that would be 3 nop insns for Power5.  */

unsigned int f (volatile unsigned int *u, unsigned int u2)
{
  *u = u2;
  return *u;
}
