/* PR target/90497 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-mno-sse -m3dnow" } */

typedef char __v8qi __attribute__ ((__vector_size__ (8)));

__v8qi
foo (__v8qi x, __v8qi y)
{
  return __builtin_ia32_pavgusb (x, y);
}
