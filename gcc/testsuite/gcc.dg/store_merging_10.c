/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

struct S {
  unsigned int b1:1;
  unsigned int b2:1;
  unsigned int b3:1;
  unsigned int b4:1;
  unsigned int b5:1;
  unsigned int b6:27;
};

struct T {
  unsigned int b1:1;
  unsigned int b2:16;
  unsigned int b3:14;
  unsigned int b4:1;
};

__attribute__((noipa)) void
foo (struct S *x)
{
  x->b1 = 1;
  x->b2 = 0;
  x->b3 = 1;
  x->b4 = 1;
  x->b5 = 0;
}

__attribute__((noipa)) void
bar (struct T *x)
{
  x->b1 = 1;
  x->b2 = 0;
  x->b4 = 0;
}

struct S s = { 0, 1, 0, 0, 1, 0x3a5f05a };
struct T t = { 0, 0xf5af, 0x3a5a, 1 };

int
main ()
{
  asm volatile ("" : : : "memory");
  foo (&s);
  bar (&t);
  asm volatile ("" : : : "memory");
  if (s.b1 != 1 || s.b2 != 0 || s.b3 != 1 || s.b4 != 1 || s.b5 != 0 || s.b6 != 0x3a5f05a)
    __builtin_abort ();
  if (t.b1 != 1 || t.b2 != 0 || t.b3 != 0x3a5a || t.b4 != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
