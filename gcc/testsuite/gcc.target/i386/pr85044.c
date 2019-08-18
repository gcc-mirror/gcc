/* { dg-do run { target cet } } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "-O2 -fcf-protection=branch" } */

void callme (void (*callback) (void));

int
main (void)
{
  int ok = 0;
  void callback (void) { ok = 1; }

  callme (&callback);

  if (!ok)
   __builtin_abort ();
  return 0;
}

__attribute__((noinline, noclone))
void
callme (void (*callback) (void))
{
  (*callback) ();
}
