/* PR middle-end/80173 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -ffixed-xmm7" } */

typedef int V __attribute__ ((vector_size (2 * sizeof (int))));

struct U { V a; V b; };

int
foo (int i)
{
  register struct U u asm ("xmm7") = {{-1, 0}, {-1, 0}};
  return u.b[i];
}

int
bar (int i)
{
  register struct U u asm ("xmm7");
  u = (struct U) {{-1, 0}, {-1, 0}};
  return u.b[i];
}
