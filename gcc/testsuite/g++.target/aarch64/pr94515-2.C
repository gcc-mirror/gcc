/* PR target/94515. Check .cfi_window_save with multiple return paths.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O2 -mbranch-protection=pac-ret" } */

volatile int zero = 0;
int global = 0;

__attribute__((noinline))
int bar(void)
{
  if (zero == 0) return 3;
  return 0;
}

__attribute__((noinline, noreturn))
void unwind (void)
{
  throw 42;
}

__attribute__((noinline, noipa))
int test(int x)
{
  if (x==1) return 2; /* This return path may not use the stack.  */
  int y = bar();
  if (y > global) global=y;
  if (y==3) unwind(); /* This return path must have RA mangle state set.  */
  return 0;
}

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
