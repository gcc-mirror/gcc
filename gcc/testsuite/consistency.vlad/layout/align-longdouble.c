#include <stdio.h>

static long double ld;

int main(void)
{
  printf ("+++Long Double alignment:\n");
  printf ("%d\n", __alignof__ (ld));
  return 0;
}
