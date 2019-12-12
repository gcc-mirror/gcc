/* PR target/90497 */
/* { dg-do compile } */
/* { dg-options "-mno-sse -mmmx" { target ia32 } } */
/* { dg-options "-mno-mmx" { target { ! ia32 } } } */

typedef char __v8qi __attribute__ ((__vector_size__ (8)));

__v8qi
foo (__v8qi x, __v8qi y)
{
  return __builtin_ia32_pcmpeqb (x, y);
}
