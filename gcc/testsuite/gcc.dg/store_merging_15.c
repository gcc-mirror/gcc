/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

struct S { unsigned char a, b; unsigned short c; unsigned char d, e, f, g; unsigned long long h; };

__attribute__((noipa)) void
f1 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = ~q->a;
  p->b = q->b;
  p->c = ~q->c;
  p->d = ~q->d;
  p->e = q->e;
  p->f = ~q->f;
  p->g = ~q->g;
}

__attribute__((noipa)) void
f2 (struct S *__restrict p, struct S *__restrict q)
{
  p->a = ~(unsigned char) (p->a & q->a);
  p->b = ((unsigned char) ~p->b) & q->b;
  p->c = p->c & (unsigned short) ~q->c;
  p->d = p->d & q->d;
  p->e = p->e & (unsigned char) ~q->e;
  p->f = p->f & (unsigned char) ~q->f;
  p->g = ~(unsigned char) (p->g & q->g);
}

struct S s = { 20, 21, 22, 23, 24, 25, 26, 27 };
struct S u = { 28, 29, 30, 31, 32, 33, 34, 35 };
struct S v = { 36, 37, 38, 39, 40, 41, 42, 43 };

int
main ()
{
  asm volatile ("" : : : "memory");
  f1 (&s, &u);
  asm volatile ("" : : : "memory");
  if (s.a != (unsigned char) ~28 || s.b != 29
      || s.c != (unsigned short) ~30 || s.d != (unsigned char) ~31
      || s.e != 32 || s.f != (unsigned char) ~33 || s.g != (unsigned char) ~34
      || s.h != 27)
    __builtin_abort ();
  f2 (&u, &v);
  asm volatile ("" : : : "memory");
  if (u.a != (unsigned char) ~(28 & 36) || u.b != (((unsigned char) ~29) & 37)
      || u.c != (30 & (unsigned short) ~38) || u.d != (31 & 39)
      || u.e != (32 & (unsigned char) ~40) || u.f != (33 & (unsigned char) ~41)
      || u.g != (unsigned char) ~(34 & 42) || u.h != 35)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
