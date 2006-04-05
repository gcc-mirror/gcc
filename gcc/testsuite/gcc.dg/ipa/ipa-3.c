/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */


/* Double constants.  */

#include <stdio.h>
int g (double b, double c)
{
  return (int)(b+c);  
}
int f (double a)
{
  if (a > 0)
    g (a, 3.1);
  else
    g (a, 3.1); 	
}
int main ()
{
  f (7.44);
  return 0;	
}


/* { dg-final { scan-ipa-dump-times "versioned function" 2 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "propagating const" 3 "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
