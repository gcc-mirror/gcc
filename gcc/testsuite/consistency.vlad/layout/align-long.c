#include <stdio.h>

static long lll;

int main(void)
{
  printf ("+++Long alignment:\n");
  printf ("%d\n", __alignof__ (lll));
  return 0;
}
