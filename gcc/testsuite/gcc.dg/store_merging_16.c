/* Only test on some 64-bit targets which do have bswap{si,di}2 patterns and
   are either big or little endian (not pdp endian).  */
/* { dg-do run { target { lp64 && { i?86-*-* x86_64-*-* powerpc*-*-* aarch64*-*-* } } } } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */

__attribute__((noipa)) void
f1 (unsigned char *p, unsigned long long q)
{
  p[0] = q;
  p[1] = q >> 8;
  p[2] = q >> 16;
  p[3] = q >> 24;
  p[4] = q >> 32;
  p[5] = q >> 40;
  p[6] = q >> 48;
  p[7] = q >> 56;
}

__attribute__((noipa)) void
f2 (unsigned char *p, unsigned long long q)
{
  p[0] = q >> 56;
  p[1] = q >> 48;
  p[2] = q >> 40;
  p[3] = q >> 32;
  p[4] = q >> 24;
  p[5] = q >> 16;
  p[6] = q >> 8;
  p[7] = q;
}

__attribute__((noipa)) void
f3 (unsigned char *__restrict p, unsigned char *__restrict q)
{
  unsigned char q3 = q[3];
  unsigned char q2 = q[2];
  unsigned char q1 = q[1];
  unsigned char q0 = q[0];
  p[0] = q3;
  p[1] = q2;
  p[2] = q1;
  p[3] = q0;
}

__attribute__((noipa)) void
f4 (unsigned char *__restrict p, unsigned char *__restrict q)
{
  p[0] = q[3];
  p[1] = q[2];
  p[2] = q[1];
  p[3] = q[0];
}

struct S { unsigned char a, b; unsigned short c; };

__attribute__((noipa)) void
f5 (struct S *__restrict p, struct S *__restrict q)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  unsigned char pa = q->c >> 8;
  unsigned char pb = q->c;
  unsigned short pc = (q->a << 8) | q->b;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  unsigned char pa = q->c;
  unsigned char pb = q->c >> 8;
  unsigned short pc = q->a | (q->b << 8);
#endif
  p->a = pa;
  p->b = pb;
  p->c = pc;
}

__attribute__((noipa)) void
f6 (struct S *__restrict p, struct S *__restrict q)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  p->a = q->c >> 8;
  p->b = q->c;
  p->c = (q->a << 8) | q->b;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  p->a = q->c;
  p->b = q->c >> 8;
  p->c = q->a | (q->b << 8);
#endif
}

struct T { unsigned long long a : 8, b : 8, c : 8, d : 8, e : 8, f : 8, g : 8, h : 8; };

__attribute__((noipa)) void
f7 (struct T *__restrict p, struct T *__restrict q)
{
  p->a = q->h;
  p->b = q->g;
  p->c = q->f;
  p->d = q->e;
  p->e = q->d;
  p->f = q->c;
  p->g = q->b;
  p->h = q->a;
}

struct S b = { 0x11, 0x12,
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
	       0x1413
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
	       0x1314
#endif
	     };
struct T e = { 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28 };

int
main ()
{
  unsigned char a[8];
  int i;
  struct S c, d;
  f1 (a, 0x0102030405060708ULL);
  for (i = 0; i < 8; ++i)
    if (a[i] != 8 - i)
      __builtin_abort ();
  f2 (a, 0x0102030405060708ULL);
  for (i = 0; i < 8; ++i)
    if (a[i] != 1 + i)
      __builtin_abort ();
  f3 (a, a + 4);
  for (i = 0; i < 8; ++i)
    if (a[i] != (i < 4 ? 8 - i : 1 + i))
      __builtin_abort ();
  f2 (a, 0x090a0b0c0d0e0f10ULL);
  f4 (a + 4, a);
  for (i = 0; i < 8; ++i)
    if (a[i] != (i < 4 ? 9 + i : 16 - i))
      __builtin_abort ();
  f5 (&c, &b);
  if (c.a != 0x14 || c.b != 0x13
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
      || c.c != 0x1112
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
      || c.c != 0x1211
#endif
      )
    __builtin_abort ();
  f6 (&d, &c);
  if (d.a != 0x11 || d.b != 0x12 || d.c != b.c)
    __builtin_abort ();
  struct T f;
  f7 (&f, &e);
  if (f.a != 0x28 || f.b != 0x27 || f.c != 0x26 || f.d != 0x25
      || f.e != 0x24 || f.f != 0x23 || f.g != 0x22 || f.h != 0x21)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 7 "store-merging" } } */
/* { dg-final { scan-tree-dump-times "__builtin_bswap64" 2 "store-merging" } } */
/* { dg-final { scan-tree-dump-times "__builtin_bswap32" 4 "store-merging" } } */
