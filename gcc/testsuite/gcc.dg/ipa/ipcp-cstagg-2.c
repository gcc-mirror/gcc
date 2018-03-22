/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fno-ipa-sra" } */

typedef struct S
{
  int (*call)(int);
} S;

static int __attribute__((noinline))
bar (const S *f, int x)
{
  x = f->call(x);
  return x;
}

extern void impossible_aa (void);

static int __attribute__((noinline))
baz (const S *f, int x)
{
  impossible_aa ();
  return bar (f, x);
}

static int
sq (int x)
{
  return x * x;
}

static const S s = {sq};

int
g (int x)
{
  return baz (&s, x);
}

int
obfuscate (int x)
{
  return baz ((S *) 0, x);
}

/* { dg-final { scan-ipa-dump "Discovered an indirect call to a known target" "cp" } } */

