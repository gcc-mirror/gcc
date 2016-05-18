/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details" } */

#define N 4

typedef int (* const A[N])(int);

typedef struct S
{
  int add_offset;
  A a;
} S;

extern const S *gs;

static int  __attribute__((noinline))
bar (const S *f, int x)
{
  gs = f;
  x = f->a[2](x);
  x = f->a[2](x);
  x = f->a[2](x);
  return x;
}

static int
zero (int x)
{
  return 0;
}

static int
addone (int x)
{
  return x + 1;
}

static int
sq (int x)
{
  return x * x;
}

static int
cube (int x)
{
  return x * x * x;
}

static const S s = {64, {zero, addone, sq, cube}};

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
