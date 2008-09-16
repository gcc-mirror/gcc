/* PR c/37529 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

void
foo ()
{
  goto *;	/* { dg-error "expected expression before" } */
}
