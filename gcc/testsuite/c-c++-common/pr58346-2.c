/* PR c/58346 */
/* { dg-do compile } */

__PTRDIFF_TYPE__
foo (int p[3][0], int q[3][0])
{
  return p - q; /* { dg-error "arithmetic on pointer to an empty aggregate" } */
}
