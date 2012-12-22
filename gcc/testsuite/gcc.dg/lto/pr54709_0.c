/* { dg-lto-do link } */
/* { dg-require-visibility "hidden" } */
/* { dg-require-effective-target fpic } */
/* { dg-extra-ld-options { -shared } } */
/* { dg-lto-options { { -fPIC -fvisibility=hidden -flto } } } */

void foo (void *p, void *q, unsigned s)
{
  __builtin_memcpy (p, q, s);
}
