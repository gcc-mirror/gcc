/* PR middle-end/47893 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=atom -fno-omit-frame-pointer -fno-strict-aliasing" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

extern void abort (void);

struct S
{
  unsigned s1:4, s2:2, s3:2, s4:2, s5:2, s6:1, s7:1, s8:1, s9:1, s10:1;
  int s11:16; unsigned s12:4; int s13:16; unsigned s14:2;
  int s15:16; unsigned s16:4; int s17:16; unsigned s18:2;
};

struct T
{
  unsigned t[3];
};

struct U
{
  unsigned u1, u2;
};

struct V;

struct W
{
  char w1[24]; struct V *w2; unsigned w3; char w4[28912];
  unsigned int w5; char w6[60];
};

struct X
{
  unsigned int x[2];
};

struct V
{
  int v1;
  struct X v2[3];
  char v3[28];
};

struct Y
{
  void *y1;
  char y2[3076];
  struct T y3[32];
  char y4[1052];
};

volatile struct S v1 = { .s15 = -1, .s16 = 15, .s17 = -1, .s18 = 3 };

__attribute__ ((noinline, noclone))
int
fn1 (int x)
{
  int r;
  __asm__ volatile ("" : "=r" (r) : "0" (1), "r" (x) : "memory");
  return r;
}

volatile int cnt;

__attribute__ ((noinline, noclone))
#ifdef __i386__
__attribute__ ((regparm (2)))
#endif
struct S
fn2 (struct Y *x, const struct X *y)
{
  if (++cnt > 1)
    abort ();
  __asm__ volatile ("" : : "r" (x), "r" (y) : "memory");
  return v1;
}

__attribute__ ((noinline, noclone))
void fn3 (void *x, unsigned y, const struct S *z, unsigned w)
{
  __asm__ volatile ("" : : "r" (x), "r" (y), "r" (z), "r" (w) : "memory");
}

volatile struct U v2;

__attribute__ ((noinline, noclone))
struct U
fn4 (void *x, unsigned y)
{
  __asm__ volatile ("" : : "r" (x), "r" (y) : "memory");
  return v2;
}

__attribute__ ((noinline, noclone))
struct S
fn5 (void *x)
{
  __asm__ volatile ("" : : "r" (x) : "memory");
  return v1;
}

volatile struct T v3;

__attribute__ ((noinline, noclone))
struct T fn6 (void *x)
{
  __asm__ volatile ("" : : "r" (x) : "memory");
  return v3;
}

__attribute__ ((noinline, noclone))
struct T fn7 (void *x, unsigned y, unsigned z)
{
  __asm__ volatile ("" : : "r" (x), "r" (y), "r" (z) : "memory");
  return v3;
}

static void
fn8 (struct Y *x, const struct V *y)
{
  void *a = x->y1;
  struct S b[4];
  unsigned i, c;
  c = fn1 (y->v1);
  for (i = 0; i < c; i++)
    b[i] = fn2 (x, &y->v2[i]);
  fn3 (a, y->v1, b, c);
}

static inline void
fn9 (void *x, struct S y __attribute__((unused)))
{
  fn4 (x, 8);
}

static void
fn10 (struct Y *x)
{
  void *a = x->y1;
  struct T b __attribute__((unused)) = fn6 (a);
  fn9 (a, fn5 (a));
}

__attribute__((noinline, noclone))
int
fn11 (unsigned int x, void *y, const struct W *z,
      unsigned int w, const char *v, const char *u)
{
  struct Y a, *t;
  unsigned i;
  t = &a;
  __builtin_memset (t, 0, sizeof *t);
  t->y1 = y;
  if (x == 0)
    {
      if (z->w3 & 1)
	fn10 (t);
      for (i = 0; i < w; i++)
	{
	  if (v[i] == 0)
	    t->y3[i] = fn7 (y, 0, u[i]);
	  else
	    return 0;
	}
    }
  else
    for (i = 0; i < w; i++)
      t->y3[i] = fn7 (y, v[i], u[i]);
  for (i = 0; i < z->w5; i++)
    fn8 (t, &z->w2[i]);
  return 0;
}

volatile int i;
const char *volatile p = "";

int
main ()
{
  struct V v = { .v1 = 0 };
  struct W w = { .w5 = 1, .w2 = &v };
  fn11 (i + 1, (void *) p, &w, i, (const char *) p, (const char *) p);
  if (cnt != 1)
    abort ();
  return 0;
}
