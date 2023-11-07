/* PR c/102989 */
/* { dg-do compile { target { bitint && exceptions } } } */
/* { dg-options "-O2 -std=gnu23 -fnon-call-exceptions -fexceptions" } */

__attribute__((noipa)) void
baz (int *p)
{
}

#if __BITINT_MAXWIDTH__ >= 575
void
foo (volatile _BitInt(575) *p, _BitInt(575) q)
{
  int a __attribute__((cleanup (baz))) = 1;
  *p = q;
}

_BitInt(575)
bar (volatile _BitInt(575) *p)
{
  int a __attribute__((cleanup (baz))) = 1;
  return *p;
}

_BitInt(575)
qux (long double l)
{
  int a __attribute__((cleanup (baz))) = 1;
  return l;
}

long double
corge (_BitInt(575) b)
{
  int a __attribute__((cleanup (baz))) = 1;
  return b;
}

_BitInt(575)
garply (_BitInt(575) x, _BitInt(575) y)
{
  int a __attribute__((cleanup (baz))) = 1;
  return x / y;
}

_BitInt(575)
waldo (_BitInt(575) x, _BitInt(575) y)
{
  int a __attribute__((cleanup (baz))) = 1;
  return x % y;
}
#endif
