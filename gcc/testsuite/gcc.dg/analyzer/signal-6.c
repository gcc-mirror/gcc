#include <stdio.h>
#include <signal.h>

extern void body_of_program(void);

/* Example of a non-static signal handler.  */

void handler(int signum)
{
  fprintf(stderr, "LOG: %i", signum); /* { dg-warning "call to 'fprintf' from within signal handler" } */
}

int main(int argc, const char *argv)
{
  signal(SIGINT, handler); /* { dg-message "registering 'handler' as signal handler" } */

  body_of_program();

  return 0;
}
