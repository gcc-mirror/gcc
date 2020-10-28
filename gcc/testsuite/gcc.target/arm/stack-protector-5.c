/* { dg-do compile } */
/* { dg-options "-fstack-protector-all -Os" } */

void __attribute__ ((noipa))
f (void)
{
  volatile int x;
  asm volatile ("" :::
		"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
		"r8", "r9", "r10", "r11", "r12", "r14");
}

/* The register clobbers above should not generate any single LDRs or STRs;
   all registers should be pushed and popped using register lists.  The only
   STRs should therefore be those associated with the stack protector tests
   themselves.

   Make sure the address of the canary is not spilled and reloaded,
   since that would give the attacker an opportunity to change the
   canary value.  */
/* { dg-final { scan-assembler-times {\tstr\t} 1 } } */
