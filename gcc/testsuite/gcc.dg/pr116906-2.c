/* { dg-require-effective-target alarm } */
/* { dg-require-effective-target signal } */
/* { dg-options "-O2 -fno-tree-ch" } */

#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

int x;

void __attribute__((noipa))
foo (int *p, unsigned n)
{
  unsigned i = 0;
  do
    {
      if (i == n)
        break;
      if (p)
        x = *p;
      i += 2;
    }
  while (1);
  x = *p;
}

void do_exit (int i)
{
  exit (0);
}

int main()
{
  struct sigaction s;
  sigemptyset (&s.sa_mask);
  s.sa_handler = do_exit;
  s.sa_flags = 0;
  sigaction (SIGALRM, &s, NULL);
  alarm (1);
  foo ((int *)0, 1);
}
