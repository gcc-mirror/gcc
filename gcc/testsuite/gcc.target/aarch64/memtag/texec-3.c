/* { dg-do run { xfail *-*-* } } */
/* { dg-require-effective-target memtag_exec } */
/* { dg-require-effective-target pthread } */
/* { dg-additional-options "-O2 -pthread" } */
#include "mte-sig.h"
#include <pthread.h>

void  __attribute__((noinline))
use (volatile unsigned char *ptr)
{
  ptr[0] = 0x41;
  ptr[1] = 0x42;
}

/* The thread in which we allocate and then we try to read out of the
   allocation pool.  */
void *thread_test (void *)
{
  volatile unsigned char *ptr = __builtin_alloca (15);

  setHandler();

  use (ptr);
  ptr[0x10] = 0x55;
  return 0;
}


int main (void)
{
  pthread_t thread;
  pthread_create (&thread, NULL, thread_test, NULL);
  pthread_join (thread, NULL);
  __builtin_printf ("End of line\n");

  return 0;
}
