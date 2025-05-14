/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-Wno-psabi -w -O2" } */

typedef unsigned V __attribute__((vector_size (64)));
typedef unsigned __int128 W __attribute__((vector_size (64)));

W a;
W b;
W c = { -0xffff, -0xffff, -0xffff, -0xffff };

static __attribute__((__noinline__, __noclone__)) W
bar (unsigned __int128 u)
{
  return u + c;
}

static inline W
foo (unsigned short s, V v)
{
  V y = (V) bar ((unsigned short) ~s);
  v >>= y;
  b ^= (W) a;
  v *= v;
  return (W) v + b;
}


int
main ()
{
  W x = foo (0, (V) { 0, 5 });
  for (unsigned i = 0; i < sizeof(x)/sizeof(x[0]); i++)
    if (x[i] != (i ? 0 : 0x1900000000))
      __builtin_abort();
  return 0;
}
