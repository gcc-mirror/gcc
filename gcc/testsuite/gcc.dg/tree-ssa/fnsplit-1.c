/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fnsplit-blocks-details" } */
#include <stdio.h>
int a[1000];

void
t(int a)
{
  if (a)
    printf ("I Am Completely Operational,"),
    printf ("And All My Circuits Are Functioning Perfectly\n");
}
int
main(void)
{
  int i;
  for (i = 0; i < 1000; i++)
    t(a[i]);
  return 0;
}
/* { dg-final { scan-tree-dump-times "Splitting function at:" 1 "fnsplit"} } */
/* { dg-final { scan-tree-dump-times "Invalid sum" 0 "fnsplit"} } */

