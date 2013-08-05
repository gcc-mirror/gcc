/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

/* Float & short constants.  */

#include <stdio.h>
int t(void);
int g (float b, short c)
{
  t();
  return c + (int)b;
}
int f (float a)
{
  int i, j = t();
  /* a is modified.  */
  if (a++ > 0)
    for (i = 0; i < j; i++)
      g (a, 3);
}
int main ()
{
  int i;
  for (i = 0; i < 100; i++)
    f (7.6);
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node" 3 "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param .1 c with const 3" "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param .0 a with const 7" "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
