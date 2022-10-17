/* Test for cleanups with pthread_cancel.  Any issue with libgcc's unwinder
   will cause this test to spin in pthread_join.  */

/* { dg-do run } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-pthread" } */

#include <pthread.h>
#include <unistd.h>
#include <stdio.h>

void *thread_loop (void *)
{
  while (1)
    {
      printf("worker: loop\n");
      sleep(1);
    }
}

int main ()
{
  pthread_t thread;

  pthread_create (&thread, 0, thread_loop, 0);
  sleep(5);
  pthread_cancel (thread);
  pthread_join (thread, 0);

  return 0;
}
