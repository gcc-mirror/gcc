/* PR target/49621 */
/* { dg-do compile } */
/* { dg-options "-O2 -maltivec" } */

#include <altivec.h>

int
foo (void)
{
  vector unsigned a, b, c;
  unsigned k = 1;

  a = (vector unsigned) { 0, 0, 0, 1 };
  b = c = (vector unsigned) { 0, 0, 0, 0 };

  a = vec_add (a, vec_splats (k));
  b = vec_add (b, a);
  c = vec_sel (c, a, b);

  if (vec_any_eq (b, c))
    return 1;

  return 0;
}
