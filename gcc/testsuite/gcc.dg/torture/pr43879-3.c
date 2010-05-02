/* { dg-do run } */
/* { dg-options "-fipa-pta" } */

typedef unsigned long ulong;

int __attribute__((noinline, noclone))
f4 (int a, int b, int c, int d, int e)
{
  return a + b + c + d + e;
}

void __attribute__((noinline, noclone))
f3 (int *p)
{
  *p = f4(1, 2, 3, 4, 5);
}

void __attribute__((noinline, noclone))
f2 ()
{
  int unused;
  f3 (&unused);
}

void __attribute__((noinline, noclone))
f1 (ulong e, ulong f)
{
  if (e > 5 || f > 5) __builtin_abort();
  f2 ();
}


int main()
{
 ulong e, f;
 for (e = 5; e > 0; e--)
   for (f = 5; f > 0; f--)
     f1(e, f);
 return 0;
}

