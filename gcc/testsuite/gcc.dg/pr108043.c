/* PR c/108043 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef void F (void);

void
foo (void)
{
  (F) {};		/* { dg-error "compound literal has function type" } */
  (F) { foo };		/* { dg-error "compound literal has function type" } */
}
