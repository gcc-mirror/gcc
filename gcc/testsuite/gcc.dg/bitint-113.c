/* PR middle-end/117354 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

#if __BITINT_MAXWIDTH__ >= 256
#define N 256
#else
#define N 64
#endif

struct S {
  unsigned char y;
  _BitInt(N) x;
} s;

__attribute__((noipa)) static void
foo (const char *, _BitInt(N))
{
}

__attribute__((noipa)) static void
bar (_BitInt(N))
{
}

static void
baz (void *p)
{
  foo ("bazbazbazb", s.x);
  __builtin_memcpy (p, &s.x, sizeof s.x);
}

int
main ()
{
  void *ptr = &s.x;
  baz (&s.x);
  bar (*(_BitInt(N) *) ptr);
}
