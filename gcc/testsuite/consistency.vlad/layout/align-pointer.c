#include <stdio.h>

static char *p;

int main(void)
{
  printf ("+++Pointer alignment:\n");
  printf ("%d\n", __alignof__ (p));
  return 0;
}
