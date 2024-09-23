/* PR target/94515. Check .cfi_negate_ra_state with multiple return paths.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O2 --save-temps" } */

volatile int zero = 0;

__attribute__((noinline, target("branch-protection=none")))
void unwind (void)
{
  if (zero == 0)
    throw 42;
}

__attribute__((noinline, noipa, target("branch-protection=pac-ret")))
int test (int z)
{
  // paciasp -> cfi_negate_ra_state: RA_no_signing -> RA_signing_SP
  if (z) {
    asm volatile ("":::"x20","x21");
    unwind ();
    // autiasp -> cfi_negate_ra_state: RA_signing_SP -> RA_no_signing
    return 1;
  } else {
    // 2nd cfi_negate_ra_state because the CFI directives are processed linearily.
    // At this point, the unwinder would believe that the address is not signed
    // due to the previous return. That's why the compiler has to emit second
    // cfi_negate_ra_state to mean that the return address is still signed.
    // cfi_negate_ra_state: RA_no_signing -> RA_signing_SP
    unwind ();
    // autiasp -> cfi_negate_ra_state: RA_signing_SP -> RA_no_signing
    return 2;
  }
}

__attribute__((target("branch-protection=none")))
int main ()
{
  try {
    test (zero);
    __builtin_abort ();
  } catch (...) {
    return 0;
  }
  __builtin_abort ();
}

/* This check only works if there are two return paths in test and
   cfi_negate_ra_state is used for both instead of cfi_remember_state
   plus cfi_restore_state.  This is currently the case with -O2.  */

/* { dg-final { scan-assembler-times {\t\.cfi_negate_ra_state\n} 4 } } */
