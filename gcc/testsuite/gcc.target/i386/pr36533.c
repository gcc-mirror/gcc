/* PR target/36533 */
/* { dg-do run { target { { i?86-*-linux* x86_64-*-linux* } && ilp32 } } } */
/* { dg-options "-Os" } */
#include <string.h>
#include <sys/mman.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

typedef struct S1
{
  unsigned long s1;
  struct S1 *s2;
  char *s3;
} S1;

typedef struct
{
  unsigned int s4;
  unsigned int s5;
  int s6;
  unsigned int *s7;
} S2;

typedef struct
{
  unsigned int s8;
  unsigned short s9;
  unsigned char s10;
  unsigned char s11;
  char s12[255];
} S3;

typedef struct
{
  unsigned int s4;
  unsigned short s13;
  unsigned short s14;
} S4;

typedef struct
{
  char s15[16];
  unsigned long s16;
} S5;

typedef struct
{
  char s15[48];
  S5 *s17;
} S6;

typedef struct
{
  S1 *s18;
} S7;

__attribute__((regparm (3), noinline)) int
fn1 (const char *x, void *y, S1 *z)
{
  asm volatile ("" : : : "memory");
  return *x + (y != 0);
}

__attribute__((regparm (3), noinline)) int
fn2 (const char *x, int y, S2 *z)
{
  asm volatile ("" : : : "memory");
  return 0;
}

static inline __attribute__ ((always_inline)) unsigned int
fn4 (unsigned short x)
{
  unsigned len = x;
  if (len == ((1 << 16) - 1))
    return 1 << 16;
  return len;
}

static inline __attribute__ ((always_inline)) S3 *
fn3 (S3 *p)
{
  return (S3 *) ((char *) p + fn4 (p->s9));
}

__attribute__((regparm (3), noinline)) int
fn5 (void)
{
  asm volatile ("" : : : "memory");
  return 0;
}

static inline __attribute__ ((always_inline)) int
fn6 (S3 *w, int x, S2 *y, S4 *z)
{
  int a = 2;
  char *b = (char *) w;
  S2 c = *y;

  while ((char *) w < b + x - 2 * sizeof (S4))
    {
      if (w->s10 && w->s8)
	{
	  fn2 (w->s12, w->s10, &c);
	  z--;
	  z->s4 = c.s4;
	  z->s13 = (unsigned short) ((char *) w - b);
	  z->s14 = w->s9;
	  a++;
	  fn5 ();
	}

      w = fn3 (w);
    }
  return a;
}

__attribute__((regparm (3), noinline)) unsigned int
test (void *u, S6 *v, S1 **w, S7 *x, S2 *y, S1 *z)
{
  unsigned b = v->s17->s16;
  unsigned a;
  S4 *c;
  unsigned d, e, f, i;

  fn1 (__func__, u, x->s18);
  c = (S4 *) (z->s3 + b);
  a = fn6 ((S3 *) (*w)->s3, b, y, c);
  c -= a;
  f = 0;
  e = 2;
  for (i = a - 1; ; i--)
    {
      if (f + (unsigned short) (c[i].s14 / 2) > b / 2)
	break;
      f += c[i].s14;
      e++;
    }
  d = a - e;
  return c[d].s4;
}

int main (void)
{
  char *p = mmap (NULL, 131072, PROT_READ | PROT_WRITE,
		  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  S1 wb, z, *w;
  S6 v;
  S7 x;
  S2 y;
  S5 vb;
  S4 s4;
  if (p == MAP_FAILED)
    return 0;
  if (munmap (p + 65536, 65536) < 0)
    return 0;
  memset (&wb, 0, sizeof (wb));
  memset (&z, 0, sizeof (z));
  memset (&v, 0, sizeof (v));
  memset (&x, 0, sizeof (x));
  memset (&y, 0, sizeof (y));
  memset (&vb, 0, sizeof (vb));
  memset (&s4, 0, sizeof (s4));
  s4.s14 = 254;
  z.s3 = p + 65536 - 2 * sizeof (S4);
  w = &wb;
  v.s17 = &vb;
  vb.s16 = 2 * sizeof (S4);
  memcpy (z.s3, &s4, sizeof (s4));
  memcpy (z.s3 + sizeof (s4), &s4, sizeof (s4));
  test ((void *) 0, &v, &w, &x, &y, &z);
  return 0;
}
