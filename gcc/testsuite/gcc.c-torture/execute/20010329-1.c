#include <limits.h>

void abort (void);
void exit (int);

int main (void)
{
  void *x = ((void *)((unsigned int)INT_MAX + 2));
  void *y = ((void *)((unsigned long)LONG_MAX + 2));
  if (x >= ((void *)((unsigned int)INT_MAX + 1))
      && x <= ((void *)((unsigned int)INT_MAX + 6))
      && y >= ((void *)((unsigned long)LONG_MAX + 1))
      && y <= ((void *)((unsigned long)LONG_MAX + 6)))
    exit (0);
  else
    abort ();
}
