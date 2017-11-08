/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

struct S { unsigned char a, b; unsigned short c; unsigned char d, e, f, g; unsigned long long h; };

__attribute__((noipa)) void
f1 (struct S *p)
{
  p->a = 1;
  p->b = 2;
  p->c = 3;
  p->d = 4;
  p->e = 5;
  p->f = 6;
  p->g = 7;
}

__attribute__((noipa)) void
f2 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = q->a;
  p->b = q->b;
  p->c = q->c;
  p->d = q->d;
  p->e = q->e;
  p->f = q->f;
  p->g = q->g;
}

__attribute__((noipa)) void
f3 (struct S *p, struct S *q)
{
  unsigned char pa = q->a;
  unsigned char pb = q->b;
  unsigned short pc = q->c;
  unsigned char pd = q->d;
  unsigned char pe = q->e;
  unsigned char pf = q->f;
  unsigned char pg = q->g;
  p->a = pa;
  p->b = pb;
  p->c = pc;
  p->d = pd;
  p->e = pe;
  p->f = pf;
  p->g = pg;
}

__attribute__((noipa)) void
f4 (struct S *p, struct S *q)
{
  unsigned char pa = p->a | q->a;
  unsigned char pb = p->b | q->b;
  unsigned short pc = p->c | q->c;
  unsigned char pd = p->d | q->d;
  unsigned char pe = p->e | q->e;
  unsigned char pf = p->f | q->f;
  unsigned char pg = p->g | q->g;
  p->a = pa;
  p->b = pb;
  p->c = pc;
  p->d = pd;
  p->e = pe;
  p->f = pf;
  p->g = pg;
}

__attribute__((noipa)) void
f5 (struct S *p, struct S *q)
{
  unsigned char pa = p->a & q->a;
  unsigned char pb = p->b & q->b;
  unsigned short pc = p->c & q->c;
  unsigned char pd = p->d & q->d;
  unsigned char pe = p->e & q->e;
  unsigned char pf = p->f & q->f;
  unsigned char pg = p->g & q->g;
  p->a = pa;
  p->b = pb;
  p->c = pc;
  p->d = pd;
  p->e = pe;
  p->f = pf;
  p->g = pg;
}

__attribute__((noipa)) void
f6 (struct S *p, struct S *q)
{
  unsigned char pa = p->a ^ q->a;
  unsigned char pb = p->b ^ q->b;
  unsigned short pc = p->c ^ q->c;
  unsigned char pd = p->d ^ q->d;
  unsigned char pe = p->e ^ q->e;
  unsigned char pf = p->f ^ q->f;
  unsigned char pg = p->g ^ q->g;
  p->a = pa;
  p->b = pb;
  p->c = pc;
  p->d = pd;
  p->e = pe;
  p->f = pf;
  p->g = pg;
}

__attribute__((noipa)) void
f7 (struct S *__restrict p, struct S *__restrict q)
{
  p->a |= q->a;
  p->b |= q->b;
  p->c |= q->c;
  p->d |= q->d;
  p->e |= q->e;
  p->f |= q->f;
  p->g |= q->g;
}

__attribute__((noipa)) void
f8 (struct S *__restrict p, struct S *__restrict q)
{
  p->a &= q->a;
  p->b &= q->b;
  p->c &= q->c;
  p->d &= q->d;
  p->e &= q->e;
  p->f &= q->f;
  p->g &= q->g;
}

__attribute__((noipa)) void
f9 (struct S *__restrict p, struct S *__restrict q)
{
  p->a ^= q->a;
  p->b ^= q->b;
  p->c ^= q->c;
  p->d ^= q->d;
  p->e ^= q->e;
  p->f ^= q->f;
  p->g ^= q->g;
}

__attribute__((noipa)) void
f10 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = ~q->a;
  p->b = ~q->b;
  p->c = ~q->c;
  p->d = ~q->d;
  p->e = ~q->e;
  p->f = ~q->f;
  p->g = ~q->g;
}

__attribute__((noipa)) void
f11 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = p->a | (unsigned char) ~q->a;
  p->b = p->b | (unsigned char) ~q->b;
  p->c = p->c | (unsigned short) ~q->c;
  p->d = p->d | (unsigned char) ~q->d;
  p->e = p->e | (unsigned char) ~q->e;
  p->f = p->f | (unsigned char) ~q->f;
  p->g = p->g | (unsigned char) ~q->g;
}

__attribute__((noipa)) void
f12 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = p->a & (unsigned char) ~q->a;
  p->b = p->b & (unsigned char) ~q->b;
  p->c = p->c & (unsigned short) ~q->c;
  p->d = p->d & (unsigned char) ~q->d;
  p->e = p->e & (unsigned char) ~q->e;
  p->f = p->f & (unsigned char) ~q->f;
  p->g = p->g & (unsigned char) ~q->g;
}

__attribute__((noipa)) void
f13 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = p->a ^ (unsigned char) ~q->a;
  p->b = p->b ^ (unsigned char) ~q->b;
  p->c = p->c ^ (unsigned short) ~q->c;
  p->d = p->d ^ (unsigned char) ~q->d;
  p->e = p->e ^ (unsigned char) ~q->e;
  p->f = p->f ^ (unsigned char) ~q->f;
  p->g = p->g ^ (unsigned char) ~q->g;
}

struct S s = { 20, 21, 22, 23, 24, 25, 26, 27 };
struct S t = { 0x71, 0x72, 0x7f04, 0x78, 0x31, 0x32, 0x34, 0xf1f2f3f4f5f6f7f8ULL };
struct S u = { 28, 29, 30, 31, 32, 33, 34, 35 };
struct S v = { 36, 37, 38, 39, 40, 41, 42, 43 };

int
main ()
{
  asm volatile ("" : : : "memory");
  f1 (&s);
  asm volatile ("" : : : "memory");
  if (s.a != 1 || s.b != 2 || s.c != 3 || s.d != 4
      || s.e != 5 || s.f != 6 || s.g != 7 || s.h != 27)
    __builtin_abort ();
  f2 (&s, &u);
  asm volatile ("" : : : "memory");
  if (s.a != 28 || s.b != 29 || s.c != 30 || s.d != 31
      || s.e != 32 || s.f != 33 || s.g != 34 || s.h != 27)
    __builtin_abort ();
  f3 (&s, &v);
  asm volatile ("" : : : "memory");
  if (s.a != 36 || s.b != 37 || s.c != 38 || s.d != 39
      || s.e != 40 || s.f != 41 || s.g != 42 || s.h != 27)
    __builtin_abort ();
  f4 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (36 | 0x71) || s.b != (37 | 0x72)
      || s.c != (38 | 0x7f04) || s.d != (39 | 0x78)
      || s.e != (40 | 0x31) || s.f != (41 | 0x32)
      || s.g != (42 | 0x34) || s.h != 27)
    __builtin_abort ();
  f3 (&s, &u);
  f5 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (28 & 0x71) || s.b != (29 & 0x72)
      || s.c != (30 & 0x7f04) || s.d != (31 & 0x78)
      || s.e != (32 & 0x31) || s.f != (33 & 0x32)
      || s.g != (34 & 0x34) || s.h != 27)
    __builtin_abort ();
  f2 (&s, &v);
  f6 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (36 ^ 0x71) || s.b != (37 ^ 0x72)
      || s.c != (38 ^ 0x7f04) || s.d != (39 ^ 0x78)
      || s.e != (40 ^ 0x31) || s.f != (41 ^ 0x32)
      || s.g != (42 ^ 0x34) || s.h != 27)
    __builtin_abort ();
  f3 (&s, &v);
  f7 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (36 | 0x71) || s.b != (37 | 0x72)
      || s.c != (38 | 0x7f04) || s.d != (39 | 0x78)
      || s.e != (40 | 0x31) || s.f != (41 | 0x32)
      || s.g != (42 | 0x34) || s.h != 27)
    __builtin_abort ();
  f3 (&s, &u);
  f8 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (28 & 0x71) || s.b != (29 & 0x72)
      || s.c != (30 & 0x7f04) || s.d != (31 & 0x78)
      || s.e != (32 & 0x31) || s.f != (33 & 0x32)
      || s.g != (34 & 0x34) || s.h != 27)
    __builtin_abort ();
  f2 (&s, &v);
  f9 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (36 ^ 0x71) || s.b != (37 ^ 0x72)
      || s.c != (38 ^ 0x7f04) || s.d != (39 ^ 0x78)
      || s.e != (40 ^ 0x31) || s.f != (41 ^ 0x32)
      || s.g != (42 ^ 0x34) || s.h != 27)
    __builtin_abort ();
  f10 (&s, &u);
  asm volatile ("" : : : "memory");
  if (s.a != (unsigned char) ~28 || s.b != (unsigned char) ~29
      || s.c != (unsigned short) ~30 || s.d != (unsigned char) ~31
      || s.e != (unsigned char) ~32 || s.f != (unsigned char) ~33
      || s.g != (unsigned char) ~34 || s.h != 27)
    __builtin_abort ();
  f3 (&s, &v);
  f11 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (36 | (unsigned char) ~0x71) || s.b != (37 | (unsigned char) ~0x72)
      || s.c != (38 | (unsigned short) ~0x7f04) || s.d != (39 | (unsigned char) ~0x78)
      || s.e != (40 | (unsigned char) ~0x31) || s.f != (41 | (unsigned char) ~0x32)
      || s.g != (42 | (unsigned char) ~0x34) || s.h != 27)
    __builtin_abort ();
  f3 (&s, &u);
  f12 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (28 & (unsigned char) ~0x71) || s.b != (29 & (unsigned char) ~0x72)
      || s.c != (30 & (unsigned short) ~0x7f04) || s.d != (31 & (unsigned char) ~0x78)
      || s.e != (32 & (unsigned char) ~0x31) || s.f != (33 & (unsigned char) ~0x32)
      || s.g != (34 & (unsigned char) ~0x34) || s.h != 27)
    __builtin_abort ();
  f2 (&s, &v);
  f13 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.a != (36 ^ (unsigned char) ~0x71) || s.b != (37 ^ (unsigned char) ~0x72)
      || s.c != (38 ^ (unsigned short) ~0x7f04) || s.d != (39 ^ (unsigned char) ~0x78)
      || s.e != (40 ^ (unsigned char) ~0x31) || s.f != (41 ^ (unsigned char) ~0x32)
      || s.g != (42 ^ (unsigned char) ~0x34) || s.h != 27)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 13 "store-merging" } } */
