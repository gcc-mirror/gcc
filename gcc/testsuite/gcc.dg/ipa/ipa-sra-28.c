/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra-details"  } */

struct S
{
  short a, b, c;
};

volatile int gc;
volatile int *arr;

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

void
baz (void)
{
  foo ((struct S *) 0);
}

void __attribute__((noipa))
confuse (void)
{
  gc = 0;
  baz ();
}

int
main (int argc, char **argv)
{
  confuse ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "Will split parameter" "sra" } } */

