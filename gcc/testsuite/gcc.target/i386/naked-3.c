/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O0" } */

#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

/* Verify that naked function traps at the end.  */

void
__attribute__((naked, noinline, noclone))
#ifdef __i386__
__attribute__((regparm(1)))
#endif
naked (int data)
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

  naked (0x12345678);

  abort ();
}
