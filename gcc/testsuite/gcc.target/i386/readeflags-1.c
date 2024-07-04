/* { dg-do run } */
/* { dg-options "-O0" } */

#include <x86intrin.h>
#ifdef DEBUG
#include <stdio.h>
#endif

extern void abort (void);

#ifdef __x86_64__
#define EFLAGS_TYPE unsigned long long int
#else
#define EFLAGS_TYPE unsigned int
#endif

__attribute__((noinline, noclone))
EFLAGS_TYPE
readeflags_test (unsigned int a, unsigned int b)
{
  volatile char x = (a == b);
  return __readeflags ();
}

int
main ()
{
  EFLAGS_TYPE flags;

  flags = readeflags_test (100, 100);

  if ((flags & 1) != 0)  /* Read CF */
    abort ();

  flags = readeflags_test (100, 101);

  if ((flags & 1) == 0)  /* Read CF */
    abort ();

#ifdef DEBUG
    printf ("PASSED\n");
#endif

  return 0;
}

