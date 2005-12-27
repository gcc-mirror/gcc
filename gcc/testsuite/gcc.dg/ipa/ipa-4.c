/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fdump-ipa-cp"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */

#include <stdio.h>
int g (int b, int c)
{
  printf ("%d %d\n", b, c);
}
int f (int a)
{
  /* First and second parameter of g gets different values.  */ 
     
  if (a > 0)
    g (a, 3);
  else
    g (a+1, 5); 	
}
int main ()
{
  f (7);
  return 0;	
}


/* { dg-final { scan-ipa-dump-times "versioned function" 1 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "propagating const" 1 "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
