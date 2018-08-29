// { dg-do run }
// { dg-require-weak "" }
// The PA HP-UX dynamic loader doesn't support unsatisfied weak symbols.
// { dg-skip-if "No unsat" { hppa*-*-hpux* } }
// { dg-skip-if "No weak unsat" { *-*-aix* } }
// The darwin loader does, but they do need to exist at link time.
// { dg-skip-if "No link unsat" { *-*-darwin* } }
// For kernel modules and static RTPs, the loader treats undefined weak
// symbols in the same way as undefined strong symbols.  The test
// therefore fails to load, so skip it.
// { dg-skip-if "" { "*-*-vxworks*" && nonpic } "*" { "-non-static" } }

extern void foo (void) __attribute__ ((weak));

int
main ()
{
  if (&foo)
    foo ();

  return 0;
}
