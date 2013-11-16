// { dg-do run { target { powerpc64*-*-linux* } } }
// { dg-options "-fexceptions -fnon-call-exceptions" }

#include <signal.h>
#include <stdlib.h>
#include <fenv.h>

#define SET_CR(R,V) __asm__ __volatile__ ("mtcrf %0,%1" : : "n" (1<<(7-R)), "r" (V<<(4*(7-R))) : "cr" #R)
#define GET_CR(R) ({ int tmp; __asm__ __volatile__ ("mfcr %0" : "=r" (tmp)); (tmp >> 4*(7-R)) & 15; })

void sighandler (int signo, siginfo_t * si, void * uc)
{
  SET_CR(2, 3);
  SET_CR(3, 2);
  SET_CR(4, 1);

  throw 0;
}

float test (float a, float b) __attribute__ ((__noinline__));
float test (float a, float b)
{
  float x;
  asm ("mtcrf %1,%2" : "=f" (x) : "n" (1 << (7-3)), "r" (0), "0" (b) : "cr3");
  return a / x;
}

int main ()
{
  struct sigaction sa;
  int status;

  sa.sa_sigaction = sighandler;
  sa.sa_flags = SA_SIGINFO;

  status = sigaction (SIGFPE, & sa, NULL);

  feenableexcept (FE_DIVBYZERO);

  SET_CR(2, 6);
  SET_CR(3, 9);
  SET_CR(4, 12);

  try {
    test (1, 0);
  }
  catch (...) {
    return GET_CR(2) != 6 || GET_CR(3) != 9 || GET_CR(4) != 12;
  }

  return 1;
}


