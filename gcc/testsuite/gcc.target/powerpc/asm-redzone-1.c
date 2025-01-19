/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
foo (void)
{
  int a = 1;
  int b = 2;
  int c = 3;
  int d = 4;
  int e = 5;
  int f = 6;
  int g = 7;
  int h = 8;
  int i = 9;
  int j = 10;
  int k = 11;
  int l = 12;
  int m = 13;
  int n = 14;
  int o = 15;
  int p = 16;
  int q = 17;
  int r = 18;
  int s = 19;
  int t = 20;
  int u = 21;
  int v = 22;
  int w = 23;
  int x = 24;
  int y = 25;
  int z = 26;
  asm volatile ("" : "+g" (a), "+g" (b), "+g" (c), "+g" (d), "+g" (e));
  asm volatile ("" : "+g" (f), "+g" (g), "+g" (h), "+g" (i), "+g" (j));
  asm volatile ("" : "+g" (k), "+g" (l), "+g" (m), "+g" (n), "+g" (o));
  asm volatile ("" : "+g" (k), "+g" (l), "+g" (m), "+g" (n), "+g" (o));
  asm volatile ("" : "+g" (p), "+g" (q), "+g" (s), "+g" (t), "+g" (u));
  asm volatile ("" : "+g" (v), "+g" (w), "+g" (y), "+g" (z));
#ifdef __PPC64__
  asm volatile ("std 1,-8(1); std 1,-16(1); std 1,-24(1); std 1,-32(1)"
		: : : "18", "19", "20", "redzone");
#elif defined(_AIX)
  asm volatile ("stw 1,-4(1); stw 1,-8(1); stw 1,-12(1); stw 1,-16(1)"
		: : : "18", "19", "20", "redzone");
#endif
  asm volatile ("" : "+g" (a), "+g" (b), "+g" (c), "+g" (d), "+g" (e));
  asm volatile ("" : "+g" (f), "+g" (g), "+g" (h), "+g" (i), "+g" (j));
  asm volatile ("" : "+g" (k), "+g" (l), "+g" (m), "+g" (n), "+g" (o));
  asm volatile ("" : "+g" (p), "+g" (q), "+g" (s), "+g" (t), "+g" (u));
  asm volatile ("" : "+g" (v), "+g" (w), "+g" (y), "+g" (z));
  return a + b + c + d + e + f + g + h + i + j + k + l + m + n;
}

__attribute__((noipa)) void
bar (char *p, long *q)
{
  (void) p;
  *q = 42;
}

int
main ()
{
  volatile int x = 256;
  long y;
  bar (__builtin_alloca (x), &y);
  if (foo () != 105)
    __builtin_abort ();
  if (y != 42)
    __builtin_abort ();
}
