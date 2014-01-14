/* PR target/59794 */
/* { dg-options "-Wno-psabi -O2 -mno-sse" } */
/* { dg-skip-if "no SSE vector" { *-*-mingw* } } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));

extern __v2df x;

extern void bar (__v2df);
void
foo (void)
{
  bar (x); /* { dg-message "warning: SSE vector argument without SSE enabled changes the ABI" } */
}
