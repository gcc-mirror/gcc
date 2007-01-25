/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */

/* Float & short constants.  */

#include <stdio.h>
int g (float b, short c)
{
  return c + (int)b;
}
int f (float a)
{
  /* a is modified.  */
  if (a++ > 0)
    g (a, 3);
}
int main ()
{
  f (7.6);
  return 0;	
}


/* { dg-final { scan-ipa-dump-times "versioned function" 2 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "replacing param with const" 2 "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
