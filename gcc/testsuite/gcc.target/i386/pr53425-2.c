/* PR target/53425 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-sse" } */
/* { dg-skip-if "no SSE vector" { x86_64-*-mingw* } } */

typedef float __v2sf __attribute__ ((__vector_size__ (8)));

extern __v2sf x;

extern void bar (__v2sf);
void
foo (void)
{
  bar (x); /* { dg-message "warning: SSE vector argument without SSE enabled changes the ABI" } */
}
