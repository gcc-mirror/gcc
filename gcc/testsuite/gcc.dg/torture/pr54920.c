/* { dg-do compile } */
/* { dg-options "-fno-common" { target { hppa*-*-hpux* } } } */

typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16)));
int a;
__m128i b;

void
fn1 ()
{
  while (1)
    b = (__m128i) (__v8hi) { a, 0, 0, 0, 0, 0 };
}
