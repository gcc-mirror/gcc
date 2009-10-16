/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

#include <stdio.h>
int g (int b, int c)
{
  printf ("%d %d\n", b, c);
}
int f (int a)
{
  /* Second parameter of g gets different values.  */
  if (a > 0)
    g (a, 3);
  else
    g (a, 5); 	
}
int main ()
{
  f (7);
  return 0;	
}


/* { dg-final { scan-ipa-dump-times "versioned function" 2 "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param a with const 7" "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param b with const 7" "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */


