/* { dg-require-effective-target lto } */
/* { dg-additional-sources "crossmodule-indir-call-topn-1a.c" } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O2 -flto -DDOJOB=1 -fdump-ipa-profile_estimate" } */

#include <stdio.h>

typedef int (*fptr) (int);
int
one (int a);

int
two (int a);

fptr table[] = {&one, &two};

int
main()
{
  int i, x;
  fptr p = &one;

  x = one (3);

  for (i = 0; i < 350000000; i++)
    {
      x = (*p) (3);
      p = table[x];
    }
  printf ("done:%d\n", x);
}

/* { dg-final-use-not-autofdo { scan-pgo-wpa-ipa-dump "2 \\(200.00%\\) speculations produced." "profile_estimate" } } */
