#include <stdio.h>

static double d;

int main(void)
{
  printf ("+++Double alignment:\n");
  printf ("%d\n", __alignof__ (d));
  return 0;
}
