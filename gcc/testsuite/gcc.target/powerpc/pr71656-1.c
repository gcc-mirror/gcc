/* Test for reload ICE arising from POWER9 Vector Dform code generation.  */
/* { dg-do compile } */
/* { dg-options "-O1 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */

typedef __attribute__((altivec(vector__))) int type_t;
type_t
func (type_t *src)
{
  asm volatile ("# force the base reg on the load below to be spilled"
                   : /* no outputs */
                   : /* no inputs */
                   : "r0", "r3", "r4", "r5", "r6", "r7",
                     "r8", "r9", "r10", "r11", "r12", "r14", "r15",
                     "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
                     "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31");
  return src[1];
}

