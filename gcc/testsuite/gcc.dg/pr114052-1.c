/* { dg-do run } */
/* { dg-require-effective-target signal } */
/* { dg-require-effective-target alarm } */
/* { dg-options "-O2" } */

#include <stdint.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

volatile int y;
void __attribute__((noipa)) put(int x)
{
  if (y)
    __builtin_printf ("%i\n", x);
}

void __attribute__((noipa)) f(void)
{
  int counter = 0;
  while (1) {
      if (counter >= 2) continue;
      put (counter++);
  }
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
  f();
}
