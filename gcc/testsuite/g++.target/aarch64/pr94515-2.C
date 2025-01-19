/* PR target/94515. Check .cfi_negate_ra_state with multiple return paths.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O2 -mbranch-protection=pac-ret" } */

volatile int zero = 0;
int global = 0;

/* This is a leaf function, so no .cfi_negate_ra_state directive is expected.  */
__attribute__((noinline))
int bar(void)
{
  if (zero == 0) return 3;
  return 0;
}

/* This function does not return normally, so the address is signed but no
 * authentication code is emitted. It means that only one CFI directive is
 * supposed to be emitted at signing time.  */
__attribute__((noinline, noreturn))
void unwind (void)
{
  throw 42;
}

/* This function has several return instructions, and alternates different RA
 * states. 4 .cfi_negate_ra_state and a .cfi_remember_state/.cfi_restore_state
 * should be emitted.
 *
 * Expected layout:
 *   A: path to return 0 without assignment to global
 *   B: global=y + branch back into A
 *   C: return 2
 *   D: unwind
 * Which gives with return pointer authentication:
 *   A: sign -> authenticate [2 negate_ra_states + remember_state for B]
 *   B: signed [restore_state]
 *   C: unsigned [negate_ra_state]
 *   D: signed [negate_ra_state]
 */
__attribute__((noinline, noipa))
int test(int x)
{
  // This return path may not use the stack. This means that the return address
  // won't be signed.
  if (x==1) return 2;

  // All the return paths of the code below must have RA mangle state set, and
  // the return address must be signed.
  int y = bar();
  if (y > global) global=y;
  if (y==3) unwind(); // authentication of the return address is not required.
  return 0; // authentication of the return address is required.
}

/* This function requires only 2 .cfi_negate_ra_state.  */
int main ()
{
  // paciasp -> cfi_negate_ra_state: RA_no_signing -> RA_signing_SP
  try {
    test (zero);
    __builtin_abort (); // authentication of the return address is not required.
  } catch (...) {
    // autiasp -> cfi_negate_ra_state: RA_signing_SP -> RA_no_signing
    return 0;
  }
  __builtin_abort (); // authentication of the return address is not required.
}
