// { dg-do run }
// { dg-require-weak "" }
// The PA HP-UX dynamic loader doesn't support unsatisfied weak symbols.
// { dg-skip-if "No unsat" { hppa*-*-hpux* } { "*" } { "" } }
// The darwin loader does, but they do need to exist at link time.
// { dg-skip-if "No link unsat" { *-*-darwin* } { "*" } { "" } }

extern void foo (void) __attribute__ ((weak));

int
main ()
{
  if (&foo)
    foo ();

  return 0;
}
