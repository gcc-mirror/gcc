/* { dg-options "-O2" } */

void (*fn) (void);

volatile int v;

__attribute__((noreturn)) void
fn0 (void)
{
  __builtin_exit (0);
}

void
fn1 (void)
{
}

__attribute__((noinline, noclone)) void
setfn (void (*x) (void))
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
  return 0;
}
