/* { dg-require-effective-target pthread }  */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>

#include "analyzer-decls.h"

int z;

static void func(void * o)
{
  (void) o;
}

int main(int    argc,
         int ** argv)
{
  struct timespec now;

  int * x;
  int   ret = 0;

  pthread_cleanup_push(func, NULL);

  while (ret != ETIMEDOUT)
    ret = rand() % 1000;

  if (ret != ETIMEDOUT)
    x = &z;

  pthread_cleanup_pop(1);

  if (ret == ETIMEDOUT)
    return 0;

  __analyzer_dump_path (); /* { dg-bogus "path" } */
  printf("x = %d\n", *x); /* { dg-bogus "use of uninitialized value 'x'" } */

  return 0;
}
