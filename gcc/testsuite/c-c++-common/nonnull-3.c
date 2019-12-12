/* PR c++/79984 */
/* { dg-do compile } */
/* { dg-options "-Wnonnull-compare" } */

enum { r = 1 };

__attribute__ ((nonnull (r))) int
f (int *p)
{
  return p == 0; /* { dg-warning "'nonnull' argument 'p' compared to NULL" } */
}
