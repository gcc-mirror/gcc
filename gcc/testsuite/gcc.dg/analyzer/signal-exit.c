/* Example of a bad call within a signal handler with replacement
   alternative.  'handler' calls 'exit', and 'exit' is not allowed
   from a signal handler.  But '_exit' is allowed.  */
/* { dg-require-effective-target signal } */

#include <signal.h>
#include <stdlib.h>

extern void body_of_program(void);

static void handler(int signum)
{
  exit(1); /* { dg-warning "call to 'exit' from within signal handler" "warning" } */
  /* { dg-message "note: '_exit' is a possible signal-safe alternative for 'exit'" "replacement note" { target *-*-* } .-1 } */
}

int main(int argc, const char *argv)
{
  signal(SIGINT, handler); /* { dg-message "registering 'handler' as signal handler" } */

  body_of_program();

  return 0;
}
