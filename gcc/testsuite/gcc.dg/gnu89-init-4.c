/* Test for GNU extensions to compound literals are giving the correct array bounds */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -W -Wall -O2" } */

int a[] = (int[4]){1, 1, 2};
int f(void)
{
  int sum = 0; int i;
  for(i = 0;i<4;i++)
    sum = a[i];
  return sum;
}
