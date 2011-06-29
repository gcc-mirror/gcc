/* { dg-options "-O2" } */

__attribute__((noreturn)) void (*fn) (void);

volatile int v;

__attribute__((noreturn)) void
fn0 (void)
{
  __builtin_exit (0);
}

__attribute__((noreturn)) void
fn1 (void)
{
  __builtin_exit (1);
}

__attribute__((noinline, noclone)) void
setfn (__attribute__((noreturn)) void (*x) (void))
{
  fn = x;
}

int
main ()
{
  int i;
  if (v < 1)
    setfn (fn0);
  else
    setfn (fn1);
  fn ();
}
