/* PR target/94514. Unwind across mixed pac-ret and non-pac-ret frames.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

__attribute__((noinline, target("branch-protection=pac-ret")))
static void do_throw (void)
{
  throw 42;
  __builtin_abort ();
}

__attribute__((noinline, target("branch-protection=none")))
static void no_pac_ret (void)
{
  do_throw ();
  __builtin_abort ();
}

int main ()
{
  try {
    no_pac_ret ();
  } catch (...) {
    return 0;
  }
  __builtin_abort ();
}
