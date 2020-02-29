/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fdump-ipa-cp-details --param=ipa-cp-eval-threshold=2"  } */

struct S
{
  int a, b, c;
};

int __attribute__((noinline)) foo (int i, struct S s);
int __attribute__((noinline)) bar (int i, struct S s);
int __attribute__((noinline)) baz (int i, struct S s);


int __attribute__((noinline))
bar (int i, struct S s)
{
  return baz (i, s);
}

int __attribute__((noinline))
baz (int i, struct S s)
{
  return foo (i, s);
}

int __attribute__((noinline))
foo (int i, struct S s)
{
  if (i == 2)
    return 0;
  else
    return s.b * s.b + bar (i - 1, s);
}

volatile int g;

void entry (void)
{
  struct S s;
  s.b = 4;
  g = bar (g, s);
}


void entry2 (void)
{
  struct S s;
  s.b = 6;
  g = baz (g, s);
}


/* { dg-final { scan-ipa-dump-times "adding an extra caller" 2 "cp" { xfail { hppa*-*-hpux* && { ! lp64 } } } } } */
