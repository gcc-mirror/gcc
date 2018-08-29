// { dg-do run { target { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } }
// { dg-require-effective-target cet }
// { dg-additional-options "-fexceptions -fnon-call-exceptions -fcf-protection" }

#include <signal.h>
#include <stdlib.h>

void sighandler (int signo, siginfo_t * si, void * uc)
{
  throw (5);
}

char * dosegv ()
{    
  * ((volatile int *)0) = 12;
  return 0;
}

int main ()
{
  struct sigaction sa;
  int status;

  sa.sa_sigaction = sighandler;
  sa.sa_flags = SA_SIGINFO;
    
  status = sigaction (SIGSEGV, & sa, NULL);
  status = sigaction (SIGBUS, & sa, NULL);

  try {
    dosegv ();
  }
  catch (int x) {
    return (x != 5);
  }

  return 1;
}
