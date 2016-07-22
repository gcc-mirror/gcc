/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details" } */

typedef struct S
{
  int add_offset;
  int (*call)(int);
} S;

extern const S *gs;

static int __attribute__((noinline))
bar (const S *f, int x)
{
  x = f->call(x);
  x = f->call(x);
  x = f->call(x);
  gs = f;
  return x;
}

static int
sq (int x)
{
  return x * x;
}

static const S s = {16, sq};

int
g (int x)
{
  return bar (&s, x);
}

int
obfuscate (int x)
{
  return bar ((S *) 0, x);
}

/* { dg-final { scan-ipa-dump-times "Discovered an indirect call to a known target" 3 "cp" } } */
