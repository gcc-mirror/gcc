/* Ensure we use the correct location when reporting where the
   signal handler was registered (PR analyzer/95188).  */

/* { dg-require-effective-target signal } */

#include <stdio.h>
#include <signal.h>

int g;
extern int foo (void);

static void
handler (int n)
{
  fprintf (stderr, "got here: %i\n", g); /* { dg-warning "call to 'fprintf' from within signal handler" } */
}

int main (int argc, char *argv[])
{
  g = foo (); /* { dg-bogus "registering" } */
  signal (SIGSEGV, handler); /* { dg-message "registering 'handler' as signal handler" } */
  return 0;
}
