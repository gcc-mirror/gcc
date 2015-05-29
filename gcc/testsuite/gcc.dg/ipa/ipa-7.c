/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

#include <stdio.h>
void send_addr (int *);
int g (int b, int c)
{
  printf ("%d %d\n", b, c);
}
int f (int a)
{
  if (a > 0)
    g (a, 3);
  else
    g (a, 5);

  send_addr (&a);
}
int main ()
{
  int i;
  for (i = 0; i < 100; i++)
    f (7);
  return 0;
}


/* { dg-final { scan-ipa-dump "Creating a specialized node of f" "cp" } } */
/* { dg-final { scan-ipa-dump-times "replacing param .. . with const 7" 1 "cp" } } */


