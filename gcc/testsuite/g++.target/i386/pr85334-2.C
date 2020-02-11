// { dg-do run }
// { dg-require-effective-target cet }
// { dg-additional-options "-fexceptions -fnon-call-exceptions -fcf-protection" }

// Delta between numbers of call stacks of pr85334-1.C and pr85334-2.C is 1.

#include <signal.h>
#include <stdlib.h>

void sighandler (int signo, siginfo_t * si, void * uc)
{
  throw (5);
}

char *
__attribute ((noinline, noclone))
dosegv ()
{    
  * ((volatile int *)0) = 12;
  return 0;
}

int
__attribute ((noinline, noclone))
func1 ()
{
  try {
    dosegv ();
  }
  catch (int x) {
    return (x != 5);
  }
  return 1;
}

int main ()
{
  struct sigaction sa;
  int status;

  sa.sa_sigaction = sighandler;
  sa.sa_flags = SA_SIGINFO;
    
  status = sigaction (SIGSEGV, & sa, NULL);
  status = sigaction (SIGBUS, & sa, NULL);

  return func1 ();
}
