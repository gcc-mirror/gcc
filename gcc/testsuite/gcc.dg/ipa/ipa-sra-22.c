/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra"  } */

struct W
{
  int a, b;
};

union U
{
  struct W w;
  long l;
};

struct Z
{
  int k;
  union U u;
};

struct S
{
  int i, j;
  struct Z z;
  char buf[64];
};

struct W gw;


static long
__attribute__((noinline))
foo (struct Z z)
{
  return z.u.l;
}

static long
__attribute__((noinline))
bar (struct S s)
{
  if (s.i > 100)
    return s.z.u.w.a;
  else
    return  foo (s.z);
}

volatile long g;

long
entry (struct S *p)
{
  struct S s = *p;

  return bar (s) | 2;
}
