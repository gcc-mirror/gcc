#include <limits.h>

void abort (void);
void exit (int);

int a[2];

void
f (int i)
{
  for (; i < INT_MAX; i++)
    {
      a[i] = -2;
      if (&a[i] == &a[1])
	break;
    }
}

int
main (void)
{
  a[0] = a[1] = 0;
  f (0);
  if (a[0] != -2 || a[1] != -2)
    abort ();
  exit (0);
}
