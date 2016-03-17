/* PR c++/70144 */
/* { dg-do compile } */

void
foo ()
{
  __builtin_constant_p (__builtin_constant_p) ?: ({ unsigned t = 0; t; });	/* { dg-error "must be directly called" } */
  __builtin_classify_type (__builtin_expect);	/* { dg-error "must be directly called" } */
}
