/* PR target/59794 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-sse" } */
/* { dg-skip-if "no SSE vector" { *-*-mingw* } } */

typedef int __v4si __attribute__ ((__vector_size__ (16)));

extern __v4si x;

__v4si
foo (void) /* { dg-error "SSE register return with SSE disabled" } */
{
  return x;
}
