/* PR debug/52132 */
/* { dg-do compile } */
/* { dg-options "-std=c99 -O2 -g" } */

#if  (__SIZEOF_INT__ < __SIZEOF_FLOAT__) \
  && (__SIZEOF_LONG__ == __SIZEOF_FLOAT__)
#define int long
#endif

int l;
void bar (void);

void
foo (int *x, float y)
{
  float b;
  union { float f; int i; } u = { .f = y };
#if  (__SIZEOF_INT__ < __SIZEOF_FLOAT__)
  u.i += 127L << 23;
#else
  u.i += 127 << 23;
#endif
  u.f = ((-1.0f / 3) * u.f + 2) * u.f - 2.0f / 3;
  b = 0.5 * (u.f + l);
  if (b >= *x)
    bar ();
}
