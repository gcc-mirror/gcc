// { dg-do run { target { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } }
// { dg-additional-options "-fexceptions -fnon-call-exceptions -fno-delete-dead-exceptions" }

#include <signal.h>
#include <stdlib.h>
#include <string.h>

static void
sighandler (int signo, siginfo_t* si, void* uc)
{
  throw (5);
}

struct S { void *p1, *p2; };

struct S v;

__attribute__ ((noinline))
int
dosegv ()
{
  struct S *p = 0;
  struct S s __attribute__((unused)) = *p;
  return 0;
}

int main ()
{
  struct sigaction sa;

  memset (&sa, 0, sizeof sa);
  sa.sa_sigaction = sighandler;
  sigaction (SIGSEGV, &sa, NULL);
  sigaction (SIGBUS, &sa, NULL);

  try {
    dosegv ();
  }
  catch (int x) {
    return (x != 5);
  }

  return 1;
}
