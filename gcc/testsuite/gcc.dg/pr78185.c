/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O" } */

#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

static char var1 = 0L;
static char *var2 = &var1;

void do_exit (int i)
{
  exit (0);
}

int main(void)
{
  struct sigaction s;
  sigemptyset (&s.sa_mask);
  s.sa_handler = do_exit;
  s.sa_flags = 0;
  sigaction (SIGALRM, &s, NULL);
  alarm (1);
  /* The following loop is infinite, the division by zero should not
     be hoisted out of it.  */
  for (; (var1 == 0 ? 0 : (100 / var1)) == *var2; );
  return 0;
}
