// PR target/85572
// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2 -msse2" }
// { dg-require-effective-target sse2_runtime }

typedef long long V __attribute__((vector_size (16)));
typedef long long W __attribute__((vector_size (32)));

__attribute__((noipa)) V
foo (V x)
{
  return x < 0 ? -x : x;
}

__attribute__((noipa)) void
bar (W *x, W *y)
{
  *y = *x < 0 ? -*x : *x;
}

int
main ()
{
  V a = { 11LL, -15LL };
  V b = foo (a);
  if (b[0] != 11LL || b[1] != 15LL)
    __builtin_abort ();
  V c = { -123456789123456LL, 654321654321654LL };
  V d = foo (c);
  if (d[0] != 123456789123456LL || d[1] != 654321654321654LL)
    __builtin_abort ();
  V e = { 0, 1 };
  V f = foo (e);
  if (f[0] != 0 || f[1] != 1)
    __builtin_abort ();
  W g = { 17LL, -32LL, -123456789123456LL, 654321654321654LL }, h;
  bar (&g, &h);
  if (h[0] != 17LL || h[1] != 32LL
      || h[2] != 123456789123456LL || h[3] != 654321654321654LL)
    __builtin_abort ();
  W i = { 0, 1, -1, 0 }, j;
  bar (&i, &j);
  if (j[0] != 0 || j[1] != 1 || j[2] != 1 || j[3] != 0)
    __builtin_abort ();
}
