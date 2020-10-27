/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mno-pcrel" } */
/* { dg-final { scan-assembler {ld 2,(24|40)\(1\)} } } */

/* From a linux kernel mis-compile of net/core/skbuff.c.  */
register unsigned long current_r1 asm ("r1");

void f (unsigned int n, void (*fun) (unsigned long))
{
  while (n--)
    (*fun) (current_r1 & -0x1000);
}
