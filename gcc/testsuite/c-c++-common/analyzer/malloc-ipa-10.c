/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

void
calls_free (void *victim) /* { dg-message "\\(3\\) entry to 'calls_free'" "event 3" } */
/* { dg-message "\\(7\\) entry to 'calls_free'" "event 7" { target *-*-* } .-1 } */
{
  free (victim); /* { dg-warning "double-'free' of 'victim'" "warning" } */
  /* { dg-message "\\(4\\) first 'free' here" "event 4" { target *-*-* } .-1 } */
  /* { dg-message "\\(8\\) second 'free' here; first 'free' was at \\(4\\)" "event 8" { target *-*-* } .-2 } */

  /* TODO: would this be better emitted at the callsite,
     for such a simple wrapper?  */
}

void do_stuff (void)
{
  /* Empty.  Irrelevant, and thus should not be expanded into in paths.  */
}

void test (void *ptr) /* { dg-message "\\(1\\) entry to 'test'" } */
{
  do_stuff ();

  calls_free (ptr); /* { dg-message "\\(2\\) calling 'calls_free' from 'test'" "event 2" } */
  /* { dg-message "\\(5\\) returning to 'test' from 'calls_free'" "event 5" { target *-*-* } .-1 } */

  do_stuff ();

  calls_free (ptr); /* { dg-message "\\(6\\) passing freed pointer 'ptr' in call to 'calls_free' from 'test'" } */

  do_stuff ();
}
