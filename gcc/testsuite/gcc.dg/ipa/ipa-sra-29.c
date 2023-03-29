/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra-details"  } */

struct S
{
  float f;
  int i;
  void *p;
};

extern struct S *gp;
int baz (float);

static int
__attribute__((noinline))
bar (struct S *p)
{
  if (p->i != 6)
    __builtin_abort ();

  return baz(p->f);
}

int
foo (void)
{
  struct S s;

  gp = &s;
  s.f = 7.4;
  s.i = 6;
  s.p = &s;

  bar (&s);
  return 0;
}

/* { dg-final { scan-ipa-dump-not "Variable constructed just to be passed to calls" "sra" } } */
