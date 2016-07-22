/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details" } */

typedef struct S
{
  int add_offset;
  int (*call)(int);
} S;

extern const S *es, *fs;

static int  __attribute__((noinline))
foo (const S f, int x)
{
  es = &f; 			/* This disables IPA-SRA */
  x = f.call(x+f.add_offset);
  x = f.call(x);
  x = f.call(x);
  return x;
}

static int  __attribute__((noinline))
bar (const S f, int x)
{
  fs = &f; 			/* This disables IPA-SRA */
  return foo (f, x);
}

static int
sq (int x)
{
  return x * x;
}

static const S s = {16, sq};

int
h (int x)
{
  return bar (s, x);
}

/* { dg-final { scan-ipa-dump-times "Discovered an indirect call to a known target" 3 "cp" } } */
