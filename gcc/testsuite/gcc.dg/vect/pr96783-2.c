/* { dg-do run } */

#include "tree-vect.h"

long a[1024];
long b[1024];

void __attribute__((noipa)) foo ()
{
  for (int i = 0; i < 256; ++i)
    {
      a[3*i] = b[1023 - 3*i - 2];
      a[3*i + 1] = b[1023 - 3*i - 1];
      a[3*i + 2] = b[1023 - 3*i];
    }
}

int main()
{
  for (int i = 0; i < 1024; ++i)
    b[i] = i;
  foo ();
  for (int i = 0; i < 256; ++i)
    if (a[3*i] != 1023 - 3*i - 2
	|| a[3*i+1] != 1023 - 3*i - 1
	|| a[3*i+2] != 1023 - 3*i)
      __builtin_abort ();
  return 0;
}
