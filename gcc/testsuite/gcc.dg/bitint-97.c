/* PR middle-end/114209 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-Og -std=c23 -fno-strict-aliasing" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

typedef signed char V __attribute__((__vector_size__(16)));
typedef _Float128 W __attribute__((__vector_size__(16)));

_Float128
foo (void *p)
{
  signed char c = *(_BitInt(128) *) p;
  _Float128 f = *(_Float128 *) p;
  W w = *(W *) p;
  signed char r = ((union { W a; signed char b[16]; }) w).b[1];
  return r + f;
}
