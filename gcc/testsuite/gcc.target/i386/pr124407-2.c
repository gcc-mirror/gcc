/* { dg-do compile } */
/* { dg-options "-Ofast -msse2" } */

__attribute__((__vector_size__(2 * sizeof(float)))) float v64f32_0;

void
foo0 (float f32_0)
{
  double v128f64_0;
  v64f32_0 -= *(float *)__builtin_memcpy(&f32_0, &v128f64_0, 1);
  foo0 (848135872);
}
