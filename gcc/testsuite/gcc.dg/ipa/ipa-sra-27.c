/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra-details"  } */

struct S
{
  short a, b, c;
};

extern int gc;
extern int *arr;

static void __attribute__((noinline))
foo (struct S *p)
{
  for (int i = 0; i < gc; i++)
    arr += p->b;
}

static void __attribute__((noinline))
baz (struct S *p)
{
  foo (p);
  gc = p->a + p->c;
}

void
bar (short a, short b, short c)
{
  struct S s;
  s.a = a;
  s.b = b;
  s.c = c;
  foo (&s);
  return;
}

void
bar2 (short a, short b, short c)
{
  struct S s;
  s.a = a;
  s.b = b;
  s.c = c;
  baz (&s);
  return;
}

/* { dg-final { scan-ipa-dump-times "Will split parameter" 2 "sra" } } */

