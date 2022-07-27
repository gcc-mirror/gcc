/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

#include "pr53533-3.c"

void
__attribute__((optimize("-O0")))
foo1 (int a[256], int b[256])
{
  int i;
  for (i = 0; i < 256; ++i)
    {
      int tmp = a[i] + 12345;
      tmp *= 914237;
      tmp += 12332;
      tmp *= 914237;
      tmp += 12332;
      tmp *= 914237;
      tmp -= 13;
      tmp *= 8000;
      b[i] = tmp;
    }
}

int main()
{
  int a[256];
  int b[256];
  int c[256];
  for (int i = 0; i != 256; i++)
    {
      b[i] = 0;
      c[i] = 1;
      a[i] = i * i - 10 * i + 33;
    }
  foo (a, b);
  foo1 (a, c);

  for (unsigned int i = 0; i != 256; i++)
    {
      if (b[i] != c[i])
	__builtin_abort ();
    }

  return 0;
}
