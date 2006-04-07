/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */

#include <stdio.h>
int g (int b, int c)
{
  printf ("%d %d\n", b, c);
}
int f (int a)
{
  /* a is modified.  */
  if (a++ > 0)
    g (a, 3);
}
int main ()
{
  f (7);
  return 0;	
}


/* { dg-final { scan-ipa-dump-times "versioned function" 2 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "propagating const" 2 "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
