/* PR c++/60689 */
/* { dg-do compile } */

struct S { char x[9]; };

void
foo (struct S *x, struct S *y, struct S *z)
{
  __atomic_exchange (x, y, z, __ATOMIC_SEQ_CST);
}
