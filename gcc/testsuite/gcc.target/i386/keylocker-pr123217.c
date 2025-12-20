/* PR target/123217 */
/* { dg-do compile } */
/* { dg-options "-mkl -O0" } */

__attribute__((__vector_size__(16))) long long v, w;

unsigned
foo (void *p, void *q)
{
  unsigned x = __builtin_ia32_encodekey128_u32 (0U, v, p);
  unsigned y = __builtin_ia32_encodekey256_u32 (0U, v, w, q);
  return x + y;
}
