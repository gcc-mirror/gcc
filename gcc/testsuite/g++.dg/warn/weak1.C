// The PA HP-UX dynamic loader doesn't support unsatisfied weak symbols.
// { dg-do run }
// { dg-require-weak "" }
// { dg-skip-if "No unsat" { hppa*-*-hpux* } { "*" } { "" } }

extern void foo (void) __attribute__ ((weak));

int
main ()
{
  if (&foo)
    foo ();

  return 0;
}
