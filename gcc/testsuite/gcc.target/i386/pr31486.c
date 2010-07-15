/* { dg-do compile } */
/* { dg-options "-msse -mno-sse2" } */
/* { dg-require-effective-target sse } */

typedef double __v2df __attribute__ ((vector_size (16)));

__v2df b = { 1.1, 1.2 };

extern __v2df a2 (__v2df a, __v2df b);

void test2 ()
{
  b = a2 (b, b);
}
