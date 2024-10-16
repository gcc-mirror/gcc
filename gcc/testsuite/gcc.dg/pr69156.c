/* PR tree-optimization/69156 */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -fpermissive -O1 -fno-tree-ccp" } */

_Bool
foo ()
{
  _Bool (*f) () = __builtin_abs;	/* { dg-warning "initialization of '_Bool \\(\\*\\)\\(\\)' from pointer to .__builtin_abs. with incompatible type .int \\\(\\\*\\\)." } */
  return f (0);
}
