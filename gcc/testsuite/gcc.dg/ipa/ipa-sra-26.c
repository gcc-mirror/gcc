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

/* { dg-final { scan-ipa-dump "Will split parameter" "sra" } } */

