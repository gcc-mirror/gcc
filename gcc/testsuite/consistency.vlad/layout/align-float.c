#include <stdio.h>

static float f;

int main(void)
{
  printf ("+++Float alignment:\n");
  printf ("%d\n", __alignof__ (f));
  return 0;
}
