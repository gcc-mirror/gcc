#include <stdio.h>

static char ccc;

int main(void)
{
  printf ("+++Char alignment:\n");
  printf ("%d\n", __alignof__ (ccc));
  return 0;
}
