/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */


/* Double constants.  */

#include <stdio.h>
void t(void);
static int g (double b, double c)
{
  t();
  return (int)(b+c);
}
static int f (double a)
{
  if (a > 0)
    g (a, 3.1);
  else
    g (a, 3.1);
}
int main ()
{
  int i;
  for (i = 0; i < 100; i++)
    f (7.44);
  return 0;
}


/* { dg-final { scan-ipa-dump "Creating a specialized node of f" "cp" } } */
/* { dg-final { scan-ipa-dump "replacing param .0 a with const 7" "cp"  } } */
/* { dg-final { scan-ipa-dump "Creating a specialized node of g" "cp" } } */
/* { dg-final { scan-ipa-dump "replacing param .0 b with const 7" "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param .1 c with const 3" "cp"  } } */
