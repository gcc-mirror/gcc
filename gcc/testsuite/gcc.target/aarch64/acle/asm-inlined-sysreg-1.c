/* { dg-do assemble { target elf } } */
/* { dg-require-effective-target aarch64_sysreg_guarding_ok } */
/* { dg-additional-options "-march=armv8-a -menable-sysreg-checking -###" } */
/* Ensure the system registers passed through inline assembly are rejected by
   assembler when guarding is enabled through "-menable-sysreg-checking" command
   line flag and proper feature flags are not passed.  */

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
/* { dg-prune-output "^(COMPILER_PATH|LIBRARY_PATH|COLLECT_GCC_OPTIONS)=.*" } */
/* { dg-message ".*\/.*as .*-menable-sysreg-checking" "assembler options" {target aarch64-*-* } 0 } */
