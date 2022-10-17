/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */

struct S { unsigned char b[2]; unsigned short c; unsigned char d[4]; unsigned long e; };

__attribute__((noipa)) void
foo (struct S *p)
{
  p->b[1] = 1;
  p->c = 23;
  p->d[0] = 4;
  p->d[1] = 5;
  p->d[2] = 6;
  p->d[3] = 7;
  p->e = 8;
}

__attribute__((noipa)) void
bar (struct S *p)
{
  p->b[1] = 9;
  p->c = 112;
  p->d[0] = 10;
  p->d[1] = 11;
}

struct S s = { { 30, 31 }, 32, { 33, 34, 35, 36 }, 37 };

int
main ()
{
  asm volatile ("" : : : "memory");
  foo (&s);
  asm volatile ("" : : : "memory");
  if (s.b[0] != 30 || s.b[1] != 1 || s.c != 23 || s.d[0] != 4 || s.d[1] != 5
      || s.d[2] != 6 || s.d[3] != 7 || s.e != 8)
    __builtin_abort ();
  bar (&s);
  asm volatile ("" : : : "memory");
  if (s.b[0] != 30 || s.b[1] != 9 || s.c != 112 || s.d[0] != 10 || s.d[1] != 11
      || s.d[2] != 6 || s.d[3] != 7 || s.e != 8)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
