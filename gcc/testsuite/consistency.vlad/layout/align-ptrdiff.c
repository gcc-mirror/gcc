#include <stdio.h>
#include <stddef.h>

static ptrdiff_t pd;

int main(void)
{
  printf ("+++ptrdiff_t alignment:\n");
  printf ("%d\n", __alignof__ (pd));
  return 0;
}
