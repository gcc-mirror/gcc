#include <stdio.h>

static short sss;

int main(void)
{
  printf ("+++Short alignment:\n");
  printf ("%d\n", __alignof__ (sss));
  return 0;
}
