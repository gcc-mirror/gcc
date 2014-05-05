/* PR tree-optimization/60502 */

typedef signed char v16i8 __attribute__ ((vector_size (16)));
typedef unsigned char v16u8 __attribute__ ((vector_size (16)));

void
foo (v16i8 *x)
{
  v16i8 m1 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  *x |= *x ^ m1;
}

void
bar (v16u8 *x)
{
  v16u8 m1 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  *x |= *x ^ m1;
}
