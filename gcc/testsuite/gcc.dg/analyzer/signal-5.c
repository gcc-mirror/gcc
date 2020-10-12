/* Example of other bad calls within a signal handler.  */
/* { dg-require-effective-target signal } */

#include <stdlib.h>
#include <signal.h>

extern void do_stuff (void *ptr);
extern void body_of_program(void);

static void handler(int signum)
{
  void *ptr = malloc (1024); /* { dg-warning "call to 'malloc' from within signal handler" } */
  do_stuff (ptr);
  free (ptr); /* { dg-warning "call to 'free' from within signal handler" } */
}

int main(int argc, const char *argv)
{
  signal(SIGINT, handler); /* { dg-message "registering 'handler' as signal handler" } */
  body_of_program();
  return 0;
}
