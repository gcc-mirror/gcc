/* { dg-require-effective-target lto } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-additional-sources "crossmodule-indir-call-topn-1a.c" } */
/* { dg-options "-O2 -flto -DDOJOB=1 -fdump-ipa-profile_estimate" } */

#ifdef FOR_AUTOFDO_TESTING
#define MAXITER 350000000
#else
#define MAXITER 3500000
#endif

#include <stdio.h>

typedef int (*fptr) (int);
int
one (int a);

int
two (int a);

fptr table[] = {&one, &two};

int foo ()
{
  int i, x;
  fptr p = &one;

  x = one (3);

  for (i = 0; i < MAXITER; i++)
    {
      x = (*p) (3);
      p = table[x];
    }
  return x;
}

int
main()
{
  int x = foo ();
  printf ("done:%d\n", x);
}

/* { dg-final-use-not-autofdo { scan-pgo-wpa-ipa-dump "2 \\(200.00%\\) speculations produced." "profile_estimate" } } */

