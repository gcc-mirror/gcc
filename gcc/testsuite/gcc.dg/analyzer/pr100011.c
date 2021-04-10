/* { dg-require-effective-target signal } */

#include <stdlib.h>
#include <signal.h>

void terminate(int sig)
{
  char buf[64] = { 0 };
  exit(1); /* { dg-warning "call to 'exit' from within signal handler" } */
}

int main(int argc, char **argv)
{
  signal(0, terminate); /* { dg-message "registering 'terminate' as signal handler" } */
  return 0;
}
