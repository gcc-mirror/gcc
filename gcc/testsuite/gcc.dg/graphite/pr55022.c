/* { dg-do run } */
/* { dg-options "-O2 -fgraphite-identity" } */

extern void abort (void);

void __attribute__((noinline,noclone))
f(int *limit, int minLen, int maxLen)
{
  int i;

  for (i = minLen; i <= maxLen; i++) {
      limit[i] = i;
  }
}

int main()
{
  int limit[256], i;
  f (limit, 0, 255);
  for (i = 0; i < 256; ++i)
    {
      if (limit[i] != i)
	abort ();
      __asm__ volatile ("" : : : "memory");
    }
  return 0;
}
