// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O1 -fschedule-insns -fselective-scheduling" }

extern int f (...);

int
testsum (void *a, int k, int n)
{
  int i, j;

  f (n / 2);
  for (i = 0; i < n; i += 8)
    for (j = 0; j < n; j += 8)
      while (k < n)
	k += 8;

  return k;
}
