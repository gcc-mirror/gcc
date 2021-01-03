/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-additional-options "-lpthread" } */

/* Just ensure that a basic threaded program works while running with hwasan.
   */

#include <pthread.h>

extern int printf (const char *, ...);
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __UINT64_TYPE__ uint64_t;

void *
successful_thread_function (void * argument)
{
    int *deref = (int *)argument;
    if (deref[0] == 100)
      deref[1] = 10;
    return (void *)0;
}

int
main (int argc, char **argv)
{
    int argument[100] = {0};
    argument[1] = 10;
    pthread_t thread_index;
    pthread_create (&thread_index, NULL, successful_thread_function, (void*)argument);

    void *retval;
    pthread_join (thread_index, &retval);

    return (uintptr_t)retval;
}
