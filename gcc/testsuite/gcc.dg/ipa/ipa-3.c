/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */


/* Double constants.  */

#include <stdio.h>
void t(void);
int g (double b, double c)
{
  t();
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
/* { dg-final { scan-ipa-dump "replacing param a with const 7" "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param b with const 7" "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param c with const 3" "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
