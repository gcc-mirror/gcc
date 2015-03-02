/* PR c/65228 */
/* { dg-do compile } */
/* { dg-options "" } */

__auto_type a = b; /* { dg-error "undeclared" } */

void
f (void)
{
  __auto_type c = d; /* { dg-error "undeclared" } */
}
