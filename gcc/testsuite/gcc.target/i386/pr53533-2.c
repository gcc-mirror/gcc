/* { dg-do run } */
/* { dg-options "-O2" } */

#include "pr53533-1.c"

void
__attribute__((optimize("-O0")))
foo1 (unsigned a[256], unsigned b[256])
{
  int i;
  for (i = 0; i < 256; ++i)
    {
      unsigned tmp = a[i] + 12345U;
      tmp *= 914237U;
      tmp += 12332U;
      tmp *= 914237U;
      tmp += 12332U;
      tmp *= 914237U;
      tmp -= 13U;
      tmp *= 8000U;
      b[i] = tmp;
    }
}

int main()
{
  unsigned int a[256];
  unsigned int b[256];
  unsigned int c[256];
  for (unsigned int i = 0; i != 256; i++)
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
