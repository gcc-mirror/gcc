#include <stdio.h>
#include <stddef.h>

static wchar_t w;

int main(void)
{
  printf ("+++wchar_t size:\n");
  printf ("%d\n", sizeof (w));
  return 0;
}
