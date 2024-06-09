/* PR middle-end/114157 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -Wno-psabi -w" } */

#if __BITINT_MAXWIDTH__ >= 256
_BitInt(256) d;
_BitInt(255) e;

void
foo (long __attribute__((vector_size (64))) s)
{
  __builtin_memmove (&d, &s, sizeof (d));
}

void
bar (_BitInt(512) x)
{
  long __attribute__((vector_size (64))) s;
  __builtin_memcpy (&s, &x, sizeof (s));
  __builtin_memcpy (&d, &s, sizeof (d));
}

void
baz (long __attribute__((vector_size (64))) s)
{
  _BitInt(256) d;
  __builtin_memmove (&d, &s, sizeof (d));
  e = d;
}

void
qux (long __attribute__((vector_size (64))) s)
{
  _BitInt(192) d;
  __builtin_memmove (&d, &s, sizeof (d));
  e = d;
}
#else
int i;
#endif

#if __BITINT_MAXWIDTH__ >= 1024
_BitInt(512)
corge (long __attribute__((vector_size (1024))) s)
{
  _BitInt(512) d;
  __builtin_memcpy (&d, &s, sizeof (d));
  return d;
}
#endif
