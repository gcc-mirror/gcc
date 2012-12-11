/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

int a[8];

void
test(unsigned int n)
{
  unsigned int i;
  unsigned int j;
  if (n<8)
    for (j=0;j<n;j++)
      {
	i = j;
	do
	  a[i+1]=a[i];
	while (i--);
      }
}
