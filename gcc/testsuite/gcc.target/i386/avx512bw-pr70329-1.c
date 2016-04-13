/* PR target/70329 */
/* { dg-do run } */
/* { dg-options "-O0 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

typedef unsigned char A __attribute__ ((vector_size (64)));
typedef unsigned int B __attribute__ ((vector_size (64)));

unsigned __attribute__ ((noinline, noclone))
foo (A a, A b, B c)
{
  a *= b;
  c[1] += a[8];
  return c[1];
}

void
TEST (void)
{
  A a = (A) { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  unsigned x = foo (a, a, (B) { 1, 2 });
  if (x != 83)
    abort ();
}
