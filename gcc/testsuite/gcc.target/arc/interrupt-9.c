/* { dg-do compile } */
/* { dg-require-effective-target archs }*/
/* { dg-options "-O0 -mirq-ctrl-saved=r0-fp" } */

/* Check if we get the move operation between fp and sp.  */

void __attribute__ ((interrupt("ilink")))
handler1 (void)
{
  asm (""
       :
       :
       : "r0", "r1", "r2", "r3", "r4",
         "r5", "r6", "r7", "r8", "r9");
}
/* { dg-final { scan-assembler "mov.*fp,sp" } } */
/* { dg-final { scan-assembler-not ".*fp,\\\[sp" } } */
