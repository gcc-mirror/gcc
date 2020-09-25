/* { dg-do compile } */
/* { dg-options "-fstack-protector-all -O2" } */

void __attribute__ ((noipa))
f (void)
{
  volatile int x;
  asm volatile ("" :::
		"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
		"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
		"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
		"x24", "x25", "x26", "x27", "x28", "x30");
}

/* The register clobbers above should not generate any single LDRs or STRs;
   all registers should be saved and restored in pairs.  The only STRs
   should be therefore be those associated with the stack protector
   tests themselves.

   Make sure the address of the canary value is not spilled and reloaded,
   since that would give the attacker an opportunity to change the
   canary value.  */
/* { dg-final { scan-assembler-times {\tstr\t} 1 } } */
