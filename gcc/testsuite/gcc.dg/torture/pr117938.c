/* { dg-do run { target { int32 && int128 } } } */
/* { dg-additional-options "-Wno-psabi --param=max-cse-insns=1" } */

typedef unsigned V __attribute__((__vector_size__(64)));
typedef unsigned __int128 W __attribute__((__vector_size__(64)));
unsigned a;
W b;
V c;
W d;

__attribute__((__noinline__))
W
bar (unsigned u, V z, W w)
{
  u *= z[5];
  return u + w;
}

W
foo (V v)
{
  unsigned g = a ? 1 : -1;
  v ^= 0 <= v;
  v <<= ((V){ bar (0, c, b)[0] } & 1);
  v >>= ((V){ g, bar (1, c, b)[0] } & 1);
  return a + b + (W) v + d;
}

int
main ()
{
  V x = (V) foo ((V) { });
  for (unsigned i = 0; i < sizeof(x)/sizeof(x[0]); i++)
    if (x[i] != (i ? 0xffffffff : 0x7fffffff))
      __builtin_abort();
}
