#include <stdio.h>
#include <stddef.h>

static wchar_t w;

int main(void)
{
  printf ("+++wchar_t alignment:\n");
  printf ("%d\n", __alignof__ (w));
  return 0;
}
