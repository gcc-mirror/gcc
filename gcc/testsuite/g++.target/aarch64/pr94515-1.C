/* PR target/94515. Check .cfi_window_save with multiple return paths.  */
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
  if (z) {
    asm volatile ("":::"x20","x21");
    unwind ();
    return 1;
  } else {
    unwind ();
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
   cfi_window_save is used for both instead of cfi_remember_state
   plus cfi_restore_state.  This is currently the case with -O2.  */

/* { dg-final { scan-assembler-times {\t\.cfi_window_save\n} 4 } } */
