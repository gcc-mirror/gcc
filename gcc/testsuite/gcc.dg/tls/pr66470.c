/* PR target/66470 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls } */

extern __thread unsigned long long a[10];
extern __thread struct S { int a, b; } b[10];

unsigned long long
foo (long x)
{
  return a[x];
}

struct S
bar (long x)
{
  return b[x];
}

#ifdef __SIZEOF_INT128__
extern __thread unsigned __int128 c[10];

unsigned __int128
baz (long x)
{
  return c[x];
}
#endif
