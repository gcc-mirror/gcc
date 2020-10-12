/* { dg-require-effective-target signal } */
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

extern void body_of_program(void);

void custom_logger(const char *msg)
{
  fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to 'fprintf' from within signal handler" } */
}

static void handler(int signum)
{
  custom_logger("got signal");
}

void test (void)
{
  void *ptr = malloc (1024);
  signal(SIGINT, handler); /* { dg-message "registering 'handler' as signal handler" } */
  body_of_program();
  free (ptr);
}
