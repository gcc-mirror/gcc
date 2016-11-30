/* PR target/78582. */
/* { dg-options "-fprofile-generate" } */
/* { dg-compile } */

#include <setjmp.h>

void reader_loop () {}

int
main (int argc, char argv, char env)
{
  int a;
  sigsetjmp (0, 0);
  argc = a = argc;
  reader_loop ();

  return 0;
}
