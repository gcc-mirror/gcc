/* { dg-do assemble { target elf} } */
/* { dg-require-effective-target aarch64_sysreg_guarding_ok } */
/* { dg-options "-save-temps -O2 -menable-sysreg-checking -march=armv8-a+gcs" } */
/* Ensure that system registers passed through inline assembly are properly
   gated on the feature flags, when the guarding is enabled through
   "-menable-sysreg-checking" command line flag.  */

#define INPUT 1

static inline void
read_write_using_sysreg (int mode)
{
  int b;

  /* write to gcspr_el0.  */
  asm volatile ("msr gcspr_el0, %[r]" ::[r] "r" (mode):);

  /* Read from gcspr_el0.  */
  asm volatile ("mrs %[r], gcspr_el0" :[r] "=r"(b)::);
}

int main()
{
  read_write_using_sysreg (INPUT);
  return 0;
}

/* { { dg-final { scan-assembler {msr\s+gcspr_el0,\s+x0} } } */
/* { { dg-final { scan-assembler {mrs\s+x0,\s+gcspr_el0} } } */
