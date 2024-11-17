#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include "tree-vect.h"

void test_finite ();
void test_infinite ();

void do_exit (int i)
{
  exit (0);
}

__attribute__((noinline))
int main1(void)
{
  test_finite ();
  struct sigaction s;
  sigemptyset (&s.sa_mask);
  s.sa_handler = do_exit;
  s.sa_flags = 0;
  sigaction (SIGALRM, &s, NULL);
  alarm (1);

  test_infinite ();

  __builtin_abort ();
  return 1;
}

int main(void)
{
  check_vect ();
  return main1();
}
