/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

struct S { unsigned int i : 8, a : 7, b : 7, j : 10, c : 15, d : 7, e : 10, f : 7, g : 9, k : 16; unsigned long long h; };

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
  unsigned short pe = q->e;
  unsigned char pf = q->f;
  unsigned short pg = q->g;
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
  unsigned short pe = p->e | q->e;
  unsigned char pf = p->f | q->f;
  unsigned short pg = p->g | q->g;
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
  unsigned short pe = p->e & q->e;
  unsigned char pf = p->f & q->f;
  unsigned short pg = p->g & q->g;
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
  unsigned short pe = p->e ^ q->e;
  unsigned char pf = p->f ^ q->f;
  unsigned short pg = p->g ^ q->g;
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

struct S s = { 72, 20, 21, 73, 22, 23, 24, 25, 26, 74, 27 };
struct S t = { 75, 0x71, 0x72, 76, 0x7f04, 0x78, 0x31, 0x32, 0x34, 77, 0xf1f2f3f4f5f6f7f8ULL };
struct S u = { 78, 28, 29, 79, 30, 31, 32, 33, 34, 80, 35 };
struct S v = { 81, 36, 37, 82, 38, 39, 40, 41, 42, 83, 43 };

int
main ()
{
  asm volatile ("" : : : "memory");
  f1 (&s);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != 1 || s.b != 2 || s.j != 73 || s.c != 3 || s.d != 4
      || s.e != 5 || s.f != 6 || s.g != 7 || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f2 (&s, &u);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != 28 || s.b != 29 || s.j != 73 || s.c != 30 || s.d != 31
      || s.e != 32 || s.f != 33 || s.g != 34 || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f3 (&s, &v);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != 36 || s.b != 37 || s.j != 73 || s.c != 38 || s.d != 39
      || s.e != 40 || s.f != 41 || s.g != 42 || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f4 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != (36 | 0x71) || s.b != (37 | 0x72) || s.j != 73
      || s.c != (38 | 0x7f04) || s.d != (39 | 0x78)
      || s.e != (40 | 0x31) || s.f != (41 | 0x32)
      || s.g != (42 | 0x34) || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f3 (&s, &u);
  f5 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != (28 & 0x71) || s.b != (29 & 0x72) || s.j != 73
      || s.c != (30 & 0x7f04) || s.d != (31 & 0x78)
      || s.e != (32 & 0x31) || s.f != (33 & 0x32)
      || s.g != (34 & 0x34) || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f2 (&s, &v);
  f6 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != (36 ^ 0x71) || s.b != (37 ^ 0x72) || s.j != 73
      || s.c != (38 ^ 0x7f04) || s.d != (39 ^ 0x78)
      || s.e != (40 ^ 0x31) || s.f != (41 ^ 0x32)
      || s.g != (42 ^ 0x34) || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f3 (&s, &v);
  f7 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != (36 | 0x71) || s.b != (37 | 0x72) || s.j != 73
      || s.c != (38 | 0x7f04) || s.d != (39 | 0x78)
      || s.e != (40 | 0x31) || s.f != (41 | 0x32)
      || s.g != (42 | 0x34) || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f3 (&s, &u);
  f8 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != (28 & 0x71) || s.b != (29 & 0x72) || s.j != 73
      || s.c != (30 & 0x7f04) || s.d != (31 & 0x78)
      || s.e != (32 & 0x31) || s.f != (33 & 0x32)
      || s.g != (34 & 0x34) || s.k != 74 || s.h != 27)
    __builtin_abort ();
  f2 (&s, &v);
  f9 (&s, &t);
  asm volatile ("" : : : "memory");
  if (s.i != 72 || s.a != (36 ^ 0x71) || s.b != (37 ^ 0x72) || s.j != 73
      || s.c != (38 ^ 0x7f04) || s.d != (39 ^ 0x78)
      || s.e != (40 ^ 0x31) || s.f != (41 ^ 0x32)
      || s.g != (42 ^ 0x34) || s.k != 74 || s.h != 27)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 9 "store-merging" } } */
