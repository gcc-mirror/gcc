#include <stdio.h>
#include <stddef.h>

static size_t sss;

int main(void)
{
  printf ("+++size_t alignment:\n");
  printf ("%d\n", __alignof__ (sss));
  return 0;
}
