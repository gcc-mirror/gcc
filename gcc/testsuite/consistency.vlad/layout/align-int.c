#include <stdio.h>

static int i;

int main(void)
{
  printf ("+++Int alignment:\n");
  printf ("%d\n", __alignof__ (i));
  return 0;
}
