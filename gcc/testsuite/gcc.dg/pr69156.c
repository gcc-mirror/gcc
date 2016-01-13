/* PR tree-optimization/69156 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-ccp" } */

_Bool
foo ()
{
  _Bool (*f) () = __builtin_abs;	/* { dg-warning "initialization from incompatible pointer type" } */
  return f (0);
}
