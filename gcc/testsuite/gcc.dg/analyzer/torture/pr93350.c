/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-Wno-psabi" } */

typedef __INT32_TYPE__   int32_t;
typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b)
{
  return (vnx4si) { 1, 2, 3, 4, 5, 6, a, b };
}
