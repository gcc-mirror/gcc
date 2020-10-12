/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O2 -fdump-ipa-profile_estimate" } */

#ifdef FOR_AUTOFDO_TESTING
#define MAXITER 350000000
#else
#define MAXITER 3500000
#endif

#include <stdio.h>

typedef int (*fptr) (int);
int
one (int a)
{
  return 1;
}

int
two (int a)
{
  return 0;
}

fptr table[] = {&one, &two};

int
main()
{
  int i, x;
  fptr p = &one;

  one (3);

  for (i = 0; i < MAXITER; i++)
    {
      x = (*p) (3);
      p = table[x];
    }
  printf ("done:%d\n", x);
}

/* { dg-final-use-not-autofdo { scan-ipa-dump "2 \\(200.00%\\) speculations produced." "profile_estimate" } } */
