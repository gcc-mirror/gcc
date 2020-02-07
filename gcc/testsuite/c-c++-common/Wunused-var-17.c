/* PR c++/93557 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunused-but-set-variable" } */

typedef int VI __attribute__((vector_size (sizeof (int) * 4)));
typedef float VF __attribute__((vector_size (sizeof (float) * 4)));

void
foo (VI *p, VF *q)
{
  VI a = (VI) { 1, 2, 3, 4 };			/* { dg-bogus "set but not used" } */
  q[0] = __builtin_convertvector (a, VF);
  VI b = p[1];					/* { dg-bogus "set but not used" } */
  q[1] = __builtin_convertvector (b, VF);
  VF c = (VF) { 5.0f, 6.0f, 7.0f, 8.0f };	/* { dg-bogus "set but not used" } */
  p[2] = __builtin_convertvector (c, VI);
  VF d = q[3];					/* { dg-bogus "set but not used" } */
  p[3] = __builtin_convertvector (d, VI);
}
