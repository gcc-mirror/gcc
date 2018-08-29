/* PR c/83595 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo ()
{
  (())((int){0);	/* { dg-error "expected expression before" } */
}
