#include <stdio.h>
#include <stddef.h>

static ptrdiff_t pd;

int main(void)
{
  printf ("+++ptrdiff_t size:\n");
  printf ("%d\n", sizeof (pd));
  return 0;
}
