/* { dg-options "(-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

MICROMIPS void
foo (int *x)
{
  asm volatile ("insn1\t%a0" :: "ZD" (&x[0]));
  asm volatile ("insn2\t%a0" :: "ZD" (&x[511]));
  asm volatile ("insn3\t%a0" :: "ZD" (&x[512]));
}

/* { dg-final { scan-assembler "\tinsn1\t0\\(" } } */
/* { dg-final { scan-assembler "\tinsn2\t2044\\(" } } */
/* { dg-final { scan-assembler-not "\tinsn3\t2048\\(" } } */
