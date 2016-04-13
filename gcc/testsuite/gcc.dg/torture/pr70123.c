/* { dg-do run } */
/* { dg-options "-fPIC" { target fpic } } */
__attribute__ ((noinline, noclone)) int
bar (int flag, const char *__restrict format, ...)
{
  asm volatile ("" : : "r" (flag), "r" (format) : "memory");
  return 0;
}

extern inline __attribute__ ((__always_inline__, __gnu_inline__, __artificial__)) int
baz (const char *__restrict fmt, ...)
{
  return bar (1, fmt, __builtin_va_arg_pack ());
}

__attribute__ ((noinline, noclone)) int
f1 (void **a, const char *b, int *c)
{
  *a = 0;
  *c = 0;
  asm volatile ("" : : "r" (&a), "r" (b), "r" (&c) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f2 (void *a, int b, int c, long d[], int *e)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d), "r" (e) : "memory");
  return 1;
}

__attribute__ ((noinline, noclone)) int
f3 (void *a, int *b)
{
  asm volatile ("" : : "r" (a), "r" (b) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f4 (void *a, const char *b, int c, int d, double *e, int f, char **g, int *h)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g), "r" (h) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f5 (void *a, long long b, int c, char **d, char **e, char **f, const char *g, long long h, int *i)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g), "r" (h) : "memory");
  asm volatile ("" : : "r" (i) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f6 (void *a, int b, int *c, int *d)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f7 (void *a, int b, long long c, long long d, long long e, double *f, int *g)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f8 (void *a, int b, long long c, long long d, long long e, char *f, const char **g, int *h, int *i)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g), "r" (h) : "memory");
  asm volatile ("" : : "r" (i) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f9 (void *a, int b, long long c, long long d, long long e, char *f, int *g)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f10 (void *a, int b, long long c, long long d, long long e, unsigned char f, unsigned char *g, int *h, int *i)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g), "r" (h) : "memory");
  asm volatile ("" : : "r" (i) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f11 (void *a, int b, long long c, long long d, long long e, long f, long *g, int *h, int *i)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g), "r" (h) : "memory");
  asm volatile ("" : : "r" (i) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f12 (void *a, int b, long long c, long long d, long long e, float f, float *g, int *h, int *i)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f), "r" (g), "r" (h) : "memory");
  asm volatile ("" : : "r" (i) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f13 (void *a, int b, long long c, long *d, long *e, int *f)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  asm volatile ("" : : "r" (e), "r" (f) : "memory");
  return 0;
}

__attribute__ ((noinline, noclone)) int
f14 (void *a, int b, int *c, int *d)
{
  asm volatile ("" : : "r" (a), "r" (b), "r" (c), "r" (d) : "memory");
  return 0;
}

volatile int a;

int
main ()
{
  int b, c, d = 0, e, f = 0;
  long g, h;
  int j = 0;
  long k, l;
  int m;
  unsigned char n[21];
  long o[21];
  float p[21];
  double q[21];
  long r[3], s = 0;
  char t[42];
  char u[21];
  char *v[3];
  const char *w[21];
  double x[3] = { 15.1515151515151515, 16.1616161616161616, 17.1717171717171717 };
  char y[40], z[81];
  void *a2;
  char *b2[10], *c2[10], *d2[10];
  char e2[] = "abcdefghijklm";
  for (g = 0; g < 21; g++)
    w[g] = "";
  f1 (&a2, y, &b);
  if (b)
    goto lab;
  c = 32;
  if (f2 (a2, c, d, r, &b) > 0)
    __builtin_strcpy (z, "12345678901234567890123478901234567");
  if (f3 (a2, &b) > 0)
    goto lab;
  if (f4 (a2, "abcdefg", 1, j, x, 14, v, &b) > 0)
    goto lab;
  for (g = 0; g < a; g++)
    goto lab;
  f5 (a2, s, f, b2, c2, d2, e2, 0L, &b);
  if (f6 (a2, -1, &e, &b) > 0)
    goto lab;
  if (b > 0)
    goto lab;
  if (f6 (a2, 1, &e, &b) > 0)
    goto lab;
  f7 (a2, 8, g, 1, g, q, &b);
  for (g = 1; g <= 20; g++)
    {
      for (h = 0; h < g; h++)
	{
	  t[h] = 0;
	  q[h] = 0;
	}
      f8 (a2, 1, g, 1, 1, u, w, &m, &b);
      for (h = 0; h < g; h++)
	baz (" %2d", t[h]);
      baz (" %d\nX", b);
      f9 (a2, 3, g, 1, g, t, &b);
      for (h = 0; h < g; h++)
	baz (" %2d", t[h]);
      f10 (a2, 4, g, 1, g, 99, n, &m, &b);
      f11 (a2, 6, g, 1, g, 99, o, &m, &b);
      f12 (a2, 7, g, 1, g, 99., p, &m, &b);
      f13 (a2, 8, g, &k, &l, &b);
    }
  f14 (a2, 1, &e, &b);
lab:
  return 0;
}
