/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx" } */
/* { dg-final { scan-assembler-not {(?n)movhpd[ \t]+} } }  */

struct X { double x[4]; };
typedef double v2df __attribute__((vector_size(16)));

extern void bar (void);
v2df __attribute__((noipa))
foo (struct X x, struct X y)
{
  bar ();
  return (v2df) {x.x[1], x.x[0] } + (v2df) { y.x[1], y.x[0] };
}
