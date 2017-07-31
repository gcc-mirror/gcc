/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2" } */

#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

int data;

/* Verify that naked function traps at the end.  */

void
__attribute__((naked, noinline, noclone))
naked (void)
{
  if (data == 0x12345678)
    return;
  asm ("ret");
}

void handler (int i)
{
  exit (0);
}

int main ()
{
  struct sigaction s;

  sigemptyset (&s.sa_mask);
  s.sa_handler = handler;
  s.sa_flags = 0;
  sigaction (SIGILL, &s, NULL);

  data = 0x12345678;
  naked ();

  abort ();
}
