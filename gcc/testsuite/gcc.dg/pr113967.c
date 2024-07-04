/* PR tree-optimization/113967 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned short W __attribute__((vector_size (4 * sizeof (short))));

void
foo (W *p)
{
  W x = *p;
  W y = {};
  __builtin_memcpy (&y, 1 + (char *) &x, sizeof (short));
  *p = y;
}
