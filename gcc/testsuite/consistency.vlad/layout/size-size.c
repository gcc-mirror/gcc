#include <stdio.h>
#include <stddef.h>

static size_t sss;

int main(void)
{
  printf ("+++size_t size:\n");
  printf ("%d\n", sizeof (sss));
  return 0;
}
