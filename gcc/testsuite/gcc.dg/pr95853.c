/* PR tree-optimization/95853 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-widening_mul" } */

#if __SIZEOF_INT128__
typedef __uint128_t W;
typedef unsigned long long T;
#else
typedef unsigned long long W;
typedef unsigned int T;
#endif

struct S { int p; T r; };

struct S
foo (T x, T y)
{
  W z = (W) x + y;
  return (struct S) { z > ~(T) 0, (T) z };
}

struct S
bar (T x)
{
  W z = (W) x + 132;
  return (struct S) { z > ~(T) 0, (T) z };
}

struct S
baz (T x, unsigned short y)
{
  W z = (W) x + y;
  return (struct S) { z > ~(T) 0, (T) z };
}

struct S
qux (unsigned short x, T y)
{
  W z = (W) x + y;
  return (struct S) { z > ~(T) 0, (T) z };
}

struct S
corge (T x, T y)
{
  T w = x + y;
  W z = (W) x + y;
  return (struct S) { z > ~(T) 0, w };
}

struct S
garple (T x, T y)
{
  W z = (W) x + y;
  T w = x + y;
  return (struct S) { z > ~(T) 0, w };
}

/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 6 "widening_mul" { target { i?86-*-* x86_64-*-* } } } } */
