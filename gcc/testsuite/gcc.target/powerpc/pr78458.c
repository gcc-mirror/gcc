/* { dg-do compile } */
/* { dg-options "-mcpu=8548 -mspe -mabi=spe" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } } */

extern void bar (void);
long double
pr78458 (long double p1)
{
  bar ();
  asm volatile ("# clobbers" :::
		"r14", "r15", "r16", "r17", "r18", "r19",
		"r20", "r21", "r22", "r23", "r24", "r25",
		"r26", "r27", "r28", "r29", "r30", "r31");
  return p1;
}
