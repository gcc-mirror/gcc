#include <stdio.h>

static bool bbb;

int main(void)
{
  printf ("+++Bool alignment:\n");
  printf ("%d\n", __alignof__ (bbb));
  return 0;
}
