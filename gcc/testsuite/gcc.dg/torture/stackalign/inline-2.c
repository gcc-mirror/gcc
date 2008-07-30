/* { dg-do run } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

int global;

void
bar (char *p, int size)
{
  __builtin_strncpy (p, "good", size);
}

static void
inline __attribute__((always_inline))
foo (int size)
{
  char *p = __builtin_alloca (size + 1);
  aligned i;

  bar (p, size);
  if (__builtin_strncmp (p, "good", size) != 0)
    {
#ifdef DEBUG
      p[size] = '\0';
      printf ("Failed: %s != good\n", p);
#endif
      abort ();
    }

  if (check_int (&i,  __alignof__(i)) != i)
    abort ();
}

int
main()
{
  foo (5);
  return 0;
}
