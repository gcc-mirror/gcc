/* Example of a bad call within a signal handler.
   'handler' calls 'custom_logger' which calls 'fprintf', and 'fprintf' is
   not allowed from a signal handler.  */
/* { dg-require-effective-target signal } */

#include <stdio.h>
#include <signal.h>

extern void body_of_program(void);

int logging = 1;

void custom_logger(const char *msg)
{
  if (logging)
    fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to 'fprintf' from within signal handler" } */
}

static void handler(int signum)
{
  custom_logger("got signal");
}

int main(int argc, const char *argv)
{
  custom_logger("started");

  signal(SIGINT, handler); /* { dg-message "registering 'handler' as signal handler" } */

  body_of_program();

  custom_logger("stopped");

  return 0;
}
